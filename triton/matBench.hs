{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Builder
import Data.Matrix.Distributed

import System.Environment
import System.IO
import Data.Functor
import Data.List.Split
import Control.Monad.Trans
import Control.Concurrent
import Network.BSD
import System.CPUTime
import Text.Printf

main = do
    Just rank' <- lookupEnv "OMPI_COMM_WORLD_RANK"
    Just size' <- lookupEnv "OMPI_COMM_WORLD_SIZE"
    let rank = read rank'
        size = read size'
    Just nodeFile <- lookupEnv "PBS_NODEFILE"
    hostname <- getHostName
    putStrLn $ hostname ++ " is " ++ show rank
    Just nodes' <- lookupEnv "NODES"
    let nodes =  map (++ ".sdsc.edu") $ splitOn "\n" nodes'
        procs = zip3 [0..] nodes (repeat 3000)

    putStrLn "Starting up"
    Just n <- lookupEnv "NUMBER"
    compute rank procs $ do
      let !(mat :: DMat Double) = constructMat (read n) rank size
      start <- lift $ getCPUTime
      !m' <- mat .* mat
      end <- lift $ getCPUTime
      let diff = (fromIntegral (end - start)) / (10^12)
      lift $ printf "Computation time: %0.6f sec\n" (diff :: Double)
      return ()
    putStrLn "DONE __________"
    return ()
