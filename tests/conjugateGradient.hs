{-# LANGUAGE ScopedTypeVariables #-}
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Builder
import Data.Matrix.Distributed
import Distribute (Distribute)
import Control.Monad

conjugateGradient :: DMat Double -> DMat Double -> DMat Double -> Distribute (DMatMessage Double) (DMat Double)
conjugateGradient mat x b = do
    rtr <- ddot b b
    let norm = sqrt rtr
    cg mat x b b rtr 0 norm
  where
    cg :: DMat Double -> DMat Double -> DMat Double -> DMat Double -> Double -> Int -> Double -> Distribute (DMatMessage Double) (DMat Double)
    cg mat x d r rtr iters norm = if (sqrt rtr) / norm < 1e-6 || iters < 100
                             then return x
                             else do
                               ad :: DMat Double <- mat .* d
                               alpha <- liftM (rtr /) $ ddot d ad
                               let x' = x ^+ alpha *# d
                                   r' = r ^- alpha *# ad
                               rtr' <- ddot r' r'
                               let beta = rtr / rtr'
                                   d = r' ^+ beta *# d
                               let relres = (sqrt rtr) / norm
                               cg mat x d r rtr' (iters + 1) norm
    ddot :: DMat Double -> DMat Double -> Distribute (DMatMessage Double) Double
    ddot x y = liftM topleft $ (transpose x) .* y

main = do
    let id = 0
        n  = 4
        mat = constructMat 1024 id n
        vec = constructVec 1024 id n
        zer = zeros 1024 id n
    compute id procs $ do
      conjugateGradient mat zer vec
    return ()
  where procs = [(0, "localhost", 4000), (1, "localhost", 3000)]
