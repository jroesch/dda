import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed

-- conjugateGradient mat b = do
--     norm <- (transpose b) .* b
--     let x = zeros (height b)
--     rtr <- (transpose b) .* b
--     cg mat x b b rtr 0 norm
--   where
--     cg mat x d r rtr iters norm = if (sqrt rtr) / norm < 1e-6 || iters < 100
--                              then return x
--                              else do
--                                ad <- mat .* b
--                                alpha <- lift (rtr /) $ ddot d ad
--                                x' <- x ^+ alpha ^* d
--                                r' <- r ^- alpha ^* ad
--                                rtr' <- ddot r' r'
--                                let beta = rtr / rtr'
--                                d <- r' ^+ beta ^* d
--                                let relres = (sqrt rtr) / norm
--                                cg mat x d r rtr' (iters + 1)
--
--     ddot x y = (transpose x) .* y

main = return ()
