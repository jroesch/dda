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

requestMatrix :: MElement a => DT.PID -> Arg -> [Q] -> Distribute (DMatMessage a) (DMat a)
requestMatrix pid dir quad = do
    DT.sendTo pid (Request dir quad)
    response <- DT.readFrom pid
    case response of
      Response res -> return $ Concrete res
      _            -> error "communication error"

respondMatrix :: MElement a => CMat a -> (DT.DProcess (DMatMessage a)) -> IO ()
respondMatrix cmat process = DT.writeP process (Response cmat)

sync :: MElement a => (DMat a, DMat a) -> Distribute (DMatMessage a) (DMat a) -> Distribute (DMatMessage a) (DMat a)
sync args action = do
    (_, reg) <- S.get
    procs <- lift $ DT.processes reg
    let numOfProcs = length procs
    semaphore <- lift $ Sem.new (length procs)
    lift $ setupResponders args procs semaphore
    result <- action
    DT.broadcast Finish
    lift $ Sem.wait semaphore numOfProcs
    return result

setupResponders :: (Container Vector a, MElement a) => (DMat a, DMat a) -> [DT.DProcess (DMatMessage a)] -> Sem.MSemN Int -> IO ()
setupResponders (l, r) procs sem =
    forM_ procs $ \p -> 
      forkIO $ Sem.with sem 1 (respondLoop p)
  where respondLoop process = do
          msg <- DT.readP process
          case msg of
            Finish -> return ()       
            Sync -> DT.writeP process Finish >> respondLoop process
            Request dir index ->
              case dir of
                L -> respondMatrix (traverseMat l index) process
                R -> respondMatrix (traverseMat r index) process

traverseMat :: MElement a => DMat a -> [Q] -> CMat a
traverseMat (Concrete c) [] = c
traverseMat (DMat _ a b c d) (i:is) = case i of
    A -> traverseMat a is
    B -> traverseMat b is
    C -> traverseMat c is
    D -> traverseMat d is        
traverseMat _ is = error "trying to index a non-dist matrix"
