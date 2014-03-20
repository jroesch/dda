{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Data.Matrix.Distributed.Sync
  ( Requests,
    sync,
    requestMatrix,
    respondMatrix
  ) where

import Data.Matrix.Distributed.Types
import Distribute (Distribute)
import qualified Distribute as DT
import qualified Control.Concurrent.MSemN as Sem
import qualified Control.Monad.State as S
import Control.Monad.Trans (lift)
import Control.Monad (forM_)
import Control.Concurrent (forkIO)
import Numeric.Container
import qualified Data.Map.Strict as Map
import Text.Printf
import Control.Monad.Trans.Reader as R
import Control.Concurrent.Chan as Chan
import Data.Serialize (encode)
import Control.Concurrent

type Requests a b = ReaderT (Chan (CMat a), MVar (Map.Map DT.PID Int)) (Distribute (DMatMessage a)) b

requestMatrix :: MElement a => DT.PID -> Arg -> [Q] -> Requests a (DMat a)
requestMatrix pid dir quad = do
    !(chan, mvar) <- R.ask
    lift $ DT.sendTo pid (Request dir quad)
    -- lift $ lift $ print "sent"
    lift $ lift $ modifyMVar_ mvar (\m ->return $ Map.adjust (+1) pid m)
    !cmat <- lift $ lift $ Chan.readChan chan
    return $ Concrete cmat

respondMatrix :: MElement a => CMat a -> (DT.DProcess (DMatMessage a)) -> IO ()
respondMatrix !cmat process = DT.writeP process (Response cmat)

sync :: MElement a => (DMat a, DMat a) -> Requests a b -> Distribute (DMatMessage a) b
sync args requests = do
    -- lift $ print "starting sync"
    (pid, reg) <- S.get
    procs <- lift $ DT.processes' reg
    let numOfProcs = length procs
    semaphore <- lift $ Sem.new (length procs)
    requests_left <- lift $ newMVar $ foldr (\(k,v) m -> Map.insert k 0 m) Map.empty procs -- keep track of how many incoming messages we have
    done_lock <- lift $ newMVar False -- whether or not we are done with monadic action
    chan <- lift $ Chan.newChan

    -- setup responders to handle incoming messages
    lift $ setupResponders pid chan args procs semaphore requests_left done_lock
    -- lift $ print "runing!"
    -- run monadic action
    !result <- runReaderT requests (chan, requests_left)
    -- lift $ print "done running"
    lift $ modifyMVar_ done_lock (\_ -> return True) -- done running our monadic computation
    DT.broadcast Finish
    lift $ Sem.wait semaphore (numOfProcs)
    return result

setupResponders :: (Container Vector a, MElement a) => DT.PID -> Chan (CMat a) -> (DMat a, DMat a) -> [(DT.PID, DT.DProcess (DMatMessage a))] -> Sem.MSemN Int -> MVar (Map.Map DT.PID Int) -> MVar Bool -> IO ()
setupResponders pid chan (l, r) procs sem req_var done_lock =
    forM_ procs $ \(k, p) ->
      forkIO $ Sem.with sem 1 (respondLoop p k)
  where respondLoop process k = do
          !msg <- DT.readP process
          -- print $ show pid ++ " got " ++ show msg
          case msg of
            Finish -> do
              -- print $ show pid ++ " finished"
              go
              where
                go = do
                  x <- S.liftM (Map.! k) $ readMVar req_var
                  y <- readMVar done_lock
                  if x == 0 && y
                  then return ()
                  else do
                        !msg <- DT.readP process
                        case msg of 
                          Response cmat -> do
                            modifyMVar_ req_var (\m -> return $ Map.adjust (\x -> x-1) k m)
                            Chan.writeChan chan cmat
                          Request dir index -> do
                            -- print $ show pid ++ "responding"
                            case dir of
                              L -> respondMatrix (traverseMat l index) process
                              R -> respondMatrix (traverseMat r index) process
                          Finish -> go
                        go
            Sync -> DT.writeP process Finish >> respondLoop process k
            Response cmat -> do
              modifyMVar_ req_var (\m -> return $ Map.adjust (\x -> x-1) k m)
              Chan.writeChan chan cmat
              respondLoop process k
            Request dir index -> do
              -- print $ show pid ++ "responding"
              case dir of
                L -> respondMatrix (traverseMat l index) process
                R -> respondMatrix (traverseMat r index) process
              respondLoop process k

traverseMat :: MElement a => DMat a -> [Q] -> CMat a
traverseMat (Concrete c) [] = c
traverseMat (DMat _ a b c d) (i:is) = case i of
    A -> traverseMat a is
    B -> traverseMat b is
    C -> traverseMat c is
    D -> traverseMat d is
traverseMat mat is = error $ printf "traverseMat: trying to index %s with %s" (show mat) (show is)
