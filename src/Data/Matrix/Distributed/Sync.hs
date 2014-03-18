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
import Data.Serialize (encode)

type Requests a b = ReaderT (Chan (CMat a)) (Distribute (DMatMessage a)) b

requestMatrix :: MElement a => DT.PID -> Arg -> [Q] -> Requests a (DMat a)
requestMatrix pid dir quad = do
    chan <- R.ask
    lift $ DT.sendTo pid (Request dir quad)
    cmat <- lift $ lift $ Chan.readChan chan
    return $ Concrete cmat

respondMatrix :: MElement a => CMat a -> (DT.DProcess (DMatMessage a)) -> IO ()
respondMatrix cmat process = DT.writeP process (Response cmat)

sync :: MElement a => (DMat a, DMat a) -> Requests a b -> Distribute (DMatMessage a) b
sync args requests = do
    (_, reg) <- S.get
    procs <- lift $ DT.processes reg
    let numOfProcs = length procs
    semaphore <- lift $ Sem.new (length procs)
    chan <- lift $ Chan.newChan
    lift $ setupResponders chan args procs semaphore
    result <- runReaderT requests chan
    DT.broadcast Finish
    lift $ Sem.wait semaphore numOfProcs
    return result

setupResponders :: (Container Vector a, MElement a) => Chan (CMat a) -> (DMat a, DMat a) -> [DT.DProcess (DMatMessage a)] -> Sem.MSemN Int -> IO ()
setupResponders chan (l, r) procs sem =
    forM_ procs $ \p ->
      forkIO $ Sem.with sem 1 (respondLoop p)
  where respondLoop process = do
          msg <- DT.readP process
          case msg of
            Finish -> return ()
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
