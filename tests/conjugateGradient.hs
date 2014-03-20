{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Builder
import Data.Matrix.Distributed
import Distribute (Distribute)
import Control.Monad
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans
import Foreign.Storable.Tuple
import System.Environment
import Control.Concurrent

conjugateGradient :: DMat Double -> DMat Double -> DMat Double -> Distribute (DMatMessage Double) (DMat Double)
conjugateGradient mat x b = do
    lift $ print "------------------"
    rtr <- ddot b b
    let norm = sqrt rtr
    lift $ print "=======./=> here"
    cg mat x b b rtr 0 norm
  where
    cg :: DMat Double -> DMat Double -> DMat Double -> DMat Double -> Double -> Int ->
          Double -> Distribute (DMatMessage Double) (DMat Double)
    cg mat x d r rtr iters norm = if iters /= 0 && ((sqrt rtr) / norm < 1e-6 || iters > 100)
                             then return x
                             else do
                               ad <- mat .* d
                               alpha <- liftM (rtr /) $ ddot d ad
                               let x' = x ^+ alpha *# d
                                   r' = r ^- alpha *# ad
                               rtr' <- ddot r' r'
                               let beta = rtr / rtr'
                                   d = r' ^+ beta *# d
                               let relres = (sqrt rtr) / norm
                               lift $ print $ "----->>" ++ show  relres
                               cg mat x' d r rtr' (iters + 1) norm
    ddot :: DMat Double -> DMat Double -> Distribute (DMatMessage Double) Double
    ddot x y = (transpose x) .* y >>= topleft

main :: IO ()
main = do
    args <- getArgs
    compute (read (args !! 0)) procs $ do
      (id, _) <- S.get
      let n   = 4
          mat = constructMat 4 id n
          vec = constructVec 4 id n
          zer = zeros 4 id n
      lift $ print $ show id ++ " " ++ show mat
      lift $ print $ show id ++ " " ++ show vec
      lift $ print $ show id ++ " " ++ show zer
      conjugateGradient mat zer vec
      return ()
    threadDelay 10000000
    return ()
  where procs = [(0, "localhost", 3000), (1, "localhost", 3001), (2, "localhost", 3002), (3, "localhost", 3003)]
