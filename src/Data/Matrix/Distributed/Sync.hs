{-# LANGUAGE FlexibleContexts #-}
module Data.Matrix.Distributed.Sync
  ( sync,
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

type Requests a = ReaderT (Chan (CMat a)) (Distribute (DMatMessage a)) (DMat a)

requestMatrix :: MElement a => DT.PID -> Arg -> [Q] -> Requests a
requestMatrix pid dir quad = do
    lift $ lift $ print "above ask"
    chan <- R.ask
    lift $ DT.sendTo pid (Request dir quad)
    (lift . lift) $ print "above read chan"
    cmat <- lift $ lift $ Chan.readChan chan
    (lift . lift) $ print "after read chan"
    return $ Concrete cmat
-- error $ printf "requestMatrix: communication error received %s instead of concrete matrix." (show response)

respondMatrix :: MElement a => CMat a -> (DT.DProcess (DMatMessage a)) -> IO ()
respondMatrix cmat process = do
  putStrLn "above writeP"
  DT.writeP process (Response cmat)
  putStrLn "below writeP"

sync :: MElement a => (DMat a, DMat a) -> Requests a -> Distribute (DMatMessage a) (DMat a)
sync args requests = do
    (_, reg) <- S.get
    procs <- lift $ DT.processes reg
    let numOfProcs = length procs
    semaphore <- lift $ Sem.new (length procs)
    chan <- lift $ Chan.newChan
    lift $ print "above setup"
    lift $ setupResponders chan args procs semaphore
    lift $ print "after setup"
    lift $ print "before run requests"
    result <- runReaderT requests chan
    lift $ print "after run requests"
    DT.broadcast Finish
    lift $ print "above wait"
    lift $ Sem.wait semaphore numOfProcs
    lift $ print "after wait"
    return result

setupResponders :: (Container Vector a, MElement a) => Chan (CMat a) -> (DMat a, DMat a) -> [DT.DProcess (DMatMessage a)] -> Sem.MSemN Int -> IO ()
setupResponders chan (l, r) procs sem =
    forM_ procs $ \p ->
      forkIO $ Sem.with sem 1 (respondLoop p)
  where respondLoop process = do
          print "1"
          msg <- DT.readP process
          print "READING A MESSAGE"
          print (msg)
          case msg of
            Finish -> return ()
            Sync -> DT.writeP process Finish >> respondLoop process
            Response cmat -> do
              print "writing to chan"
              Chan.writeChan chan cmat
              print "done with chan"
              respondLoop process
            Request dir index -> do
              case dir of
                L -> respondMatrix (traverseMat l index) process
                R -> respondMatrix (traverseMat r index) process
              print "about to loop again"
              respondLoop process

traverseMat :: MElement a => DMat a -> [Q] -> CMat a
traverseMat (Concrete c) [] = c
traverseMat (DMat _ a b c d) (i:is) = case i of
    A -> traverseMat a is
    B -> traverseMat b is
    C -> traverseMat c is
    D -> traverseMat d is
traverseMat _ is = error "trying to index a non-dist matrix"
