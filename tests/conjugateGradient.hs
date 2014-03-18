{-# LANGUAGE ScopedTypeVariables #-}
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Builder
import Data.Matrix.Distributed
import Distribute (Distribute)
import Control.Monad

conjugateGradient :: (Floating a, MElement a) => DMat a -> DMat a -> DMat a -> Distribute (DMatMessage a) (DMat a)
conjugateGradient mat x b = do
    rtr <- ddot b b
    let norm = sqrt rtr
    cg mat x b b rtr 0 norm
  where
    cg mat x d r rtr iters norm = if (sqrt rtr) / norm < 1e-6 || iters < 100
                             then return x
                             else do
                               ad <- mat .* b
                               alpha <- liftM (rtr /) $ ddot d ad
                               let x' = x ^+ alpha *# d
                                   r' = r ^- alpha *# ad
                               rtr' <- ddot r' r'
                               let beta = rtr / rtr'
                                   d = r' ^+ beta *# d
                               let relres = (sqrt rtr) / norm
                               cg mat x d r rtr' (iters + 1)

    ddot x y = liftM topleft $ (transpose x) .* y

main = do
    let id = 0
        n  = 4
        mat = constructMat 1024 id n
        vec = constructVec 1024 id n
        zer = zeros 1024 id n
    conjugateGradient mat zer vec
    return ()
