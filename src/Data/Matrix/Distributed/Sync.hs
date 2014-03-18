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
import qualified Data.Vector
import Text.Printf
import Control.Monad.Trans.Reader as R
import Control.Concurrent.Chan as Chan
import Data.Serialize (encode)
import Control.Concurrent

type Requests a b = ReaderT (Chan (CMat a)) (Distribute (DMatMessage a)) b

requestMatrix :: MElement a => DT.PID -> Arg -> [Q] -> Requests a (DMat a)
requestMatrix pid dir quad = do
    !chan <- R.ask
    lift $ DT.sendTo pid (Request dir quad)
    !cmat <- lift $ lift $ Chan.readChan chan
    return $ Concrete cmat

respondMatrix :: MElement a => CMat a -> (DT.DProcess (DMatMessage a)) -> IO ()
respondMatrix !cmat process = DT.writeP process (Response cmat)

sync :: MElement a => (DMat a, DMat a) -> Requests a b -> Distribute (DMatMessage a) b
sync args requests = do
    lift $ print "starting sync"
    (pid, reg) <- S.get
    procs <- lift $ DT.processes reg
    let numOfProcs = length procs
    semaphore <- lift $ Sem.new (length procs)
    chan <- lift $ Chan.newChan
    lift $ setupResponders pid chan args procs semaphore
    lift $ yield
    lift $ print 5
    !result <- runReaderT requests chan -- hanging here
    lift $ yield
    lift $ yield
    lift $ yield
    lift $ print 6
    DT.broadcast Finish
    lift $ yield
    lift $ print 7
    lift $ print $ show pid ++ " broadcasted finish"
    lift $ Sem.wait semaphore (numOfProcs)
    lift $ print "done syncing"
    return result

setupResponders :: (Container Vector a, MElement a) => DT.PID -> Chan (CMat a) -> (DMat a, DMat a) -> [DT.DProcess (DMatMessage a)] -> Sem.MSemN Int -> IO ()
setupResponders pid chan (l, r) procs sem =
    forM_ procs $ \p ->
      forkIO $ Sem.with sem 1 (respondLoop p)
  where respondLoop process = do
          !msg <- DT.readP process
          print $ show pid ++ " got " ++ show msg
          case msg of
            Finish -> do
              print $ show pid ++ " finished"
              -- respondLoop process
              return ()
            Sync -> DT.writeP process Finish >> respondLoop process
            Response cmat -> do
              Chan.writeChan chan cmat
              respondLoop process
            Request dir index -> do
              case dir of
                L -> respondMatrix (traverseMat l index) process
                R -> respondMatrix (traverseMat r index) process
              respondLoop process

traverseMat :: MElement a => DMat a -> [Q] -> CMat a
traverseMat (Concrete c) [] = c
traverseMat (DMat _ a b c d) (i:is) = case i of
    A -> traverseMat a is
    B -> traverseMat b is
    C -> traverseMat c is
    D -> traverseMat d is
traverseMat mat is = error $ printf "traverseMat: trying to index %s with %s" (show mat) (show is)
