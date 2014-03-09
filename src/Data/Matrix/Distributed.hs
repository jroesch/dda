{-# LANGUAGE FlexibleContexts #-}

module Data.Matrix.Distributed ()
where

import Data.Word
import Data.Vector as V
import Data.Vector.Mutable as M
import Data.Vector.Hybrid as H
import qualified Sparse.Matrix as S
import qualified Data.Packed as D
import Foreign.Storable
import Numeric.Container
import Control.Monad
import Control.Lens

{-
--                 width height mat
-- each node stores a row of the matrix
data SMat a = SMat !Word !Word (Vector Matrix)

toXY :: !Word -> !Word -> !(Word, Word)
toXY i s = (i `div` s, i `mod` s)

toI :: !(Word, Word) -> !Word -> !Word
toI (x, y) s = y * s + x

matMul :: SMat a -> Smat a -> IO (SMat a)
matMul m1@(SMat width1 height1 vm1) m2@(SMat width2 height2 vm2) = do
    vm <- V.generateM numNodes (\i -> go 0 i Nothing)
    return $ SMat width1 height1 vm
  where
    -- TODO: make this a fold
    go i x m = case i < numNodes of
              True  -> do
                let lm = vm1 ! i
                rm <- if i == id then vm2 ! i else get (x, i)
                case m of
                  Just mm -> go (i + 1) (mm + (lm * rm))
                  Nothing -> go (i + 1) (lm * rm)
              False -> m

matAdd :: SMat a -> Smat a -> IO (SMat a)
matAdd m1@(SMat width1 height1 vm1) m2@(SMat width2 height2 vm2) =
    return $ SMat width1 height1 $ V.zipWith (+) vm1 vm2
-}

sdMul :: (Num a, S.Eq0 a, Storable a) => S.Mat a -> D.Matrix a -> S.Mat a
sdMul a b = a & S._Mat %~ H.map (\(S.Key r c, d) -> (S.Key r c, d * (b @@> (fromIntegral r, fromIntegral c))))

sdAdd :: (Num a, S.Eq0 a, Storable a) => S.Mat a -> D.Matrix a -> D.Matrix a
sdAdd a b = V.modify (\v -> H.forM_ sparse (\(S.Key r c, val) -> write v (c + r * width) val + (v V.! c + r * width)))
  where
    sparse = a ^. S._Mat
    -- D.Matrix width height dense order = b -- TODO: handle matrix order
    D.Matrix { irows=width, icols = height, xdat = dense , order = oder } = b

-- sparse matrix based on peano ordering
--                   top left   top right  bottom left bottom right
data PSMat a = PNode !(PSMat a) !(PSMat a) !(PSMat a) !(PSMat a)
             | Dense (D.Matrix a)
             | Sparse (S.Mat a)
             | Zero
  deriving (Show)

sMatAdd :: (S.Eq0 a, Num a, Container Matrix a) => PSMat a -> PSMat a -> PSMat a
sMatAdd a b = case (a, b) of
                (Zero, r) -> r
                (l, Zero) -> l
                (Sparse left, Sparse right) -> Sparse $ left + right
                (Dense left, Dense right) -> Dense $ left `add` right
                (PNode l1 l2 l3 l4, PNode r1 r2 r3 r4) -> PNode (sMatAdd l1 r1)
                                                                (sMatAdd l2 r2)
                                                                (sMatAdd l3 r3)
                                                                (sMatAdd l4 r4)
                (Sparse left, Dense right) -> Dense $ sdAdd left right
                (Dense left, Sparse right) -> Dense $ (flip sdAdd) left right
                otherwise -> Zero


sMatMul :: (S.Eq0 a, Num a, Product a, Container Matrix a) => PSMat a -> PSMat a -> PSMat a
sMatMul a b = case (a, b) of
                (Zero, _) -> Zero
                (_, Zero) -> Zero
                (Sparse left, Sparse right) -> Sparse $ left * right
                (Dense left, Dense right) -> Dense $ left `multiply` right
                (PNode l1 l2 l3 l4, PNode r1 r2 r3 r4) -> PNode (sMatAdd (sMatMul l1 r1) (sMatMul l2 r3))
                                                                (sMatAdd (sMatMul l1 r2) (sMatMul l2 r4))
                                                                (sMatAdd (sMatMul l3 r1) (sMatMul l4 r3))
                                                                (sMatAdd (sMatMul l3 r2) (sMatMul l4 r4))
                (Sparse left, Dense right) -> Sparse $ sdMul left right
                (Dense left, Sparse right) -> Sparse $ (flip sdMul) left right
                otherwise -> Zero -- TODO: fix

{-
-- distributed sparse matrix
data DPSMat a = DPSMat !Word !Word (PSMat a)

-- distributed matrix multiply
dMalMut 
-}
