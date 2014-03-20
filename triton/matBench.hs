{-# LANGUAGE ScopedTypeVariables #-}
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Builder
import Data.Matrix.Distributed

import System.Environment
import System.IO
import Data.Functor
import Data.List.Split
import Control.Monad.Trans
import Control.Concurrent

main = do
    Just rank' <- lookupEnv "OMPI_COMM_WORLD_RANK"
    Just size' <- lookupEnv "OMPI_COMM_WORLD_SIZE"
    let rank = read rank'
        size = read size'
    Just nodeFile <- lookupEnv "PBS_NODEFILE"
    -- hostname <- getHostName
    -- putStrLn $ hostname ++ " is " ++ show rank
    Just nodes' <- lookupEnv "NODES"
    let nodes =  map (++ ".sdsc.edu") $ splitOn "\n" nodes'
        procs = zip3 [0..] nodes (repeat 3000)

    compute rank procs $ do
      let n   = 4
          mat :: DMat Double = constructMat 4 rank size
      lift $ print $ show rank ++ " " ++ show mat
      m' <- mat .* mat
      lift $ print m'
      return ()
    threadDelay 10000000
    return ()
