{-# LANGUAGE FlexibleContexts #-}
module Data.Matrix.Distributed where

import Control.Applicative
import Data.Matrix.Types
import Data.Word
import Data.Bits
import Data.Vector as V
import Data.Vector.Mutable as M
import Data.Vector.Hybrid as H
import qualified Sparse.Matrix as S
import qualified Data.Packed as D
import Foreign.Storable
import Numeric.Container
import Control.Monad as CM
import Control.Lens
import Distribute (Distribute)
import qualified Distribute as DT
import qualified Data.Serialize as Cereal

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

opOnNonZeros :: (a -> b) -> DMat a -> DMat b
opOnNonSeros op mat = case mat of
                        Concrete (Dense smat) -> Concrete $ D.mapMatrix op smat
                        Concrete (Sparse smat) -> Concrete $ smat & S._Mat %~ H.map (op . snd)
                        Concrete Zero -> Concrete Zero
                        Remote pid -> Remote pid
                        DMat tl tr bl br = DMat (opOnNonZeros tl) (opOnNonZeros tr)
                                                (opOnNonZeros bl) (opOnNonZeros br)

sdMult :: (Num a, S.Eq0 a, Storable a) => S.Mat a -> D.Matrix a -> S.Mat a
sdMult a b = a & S._Mat %~ H.map (\(S.Key r c, d) -> (S.Key r c, d * (b @@> (fromIntegral r, fromIntegral c))))

-- TODO: am i accidentaly transposing
sdAdd :: (Num a, S.Eq0 a, Storable a) => S.Mat a -> D.Matrix a -> D.Matrix a
sdAdd a b = (height><width) $ V.toList resV
  where
    sparse = a ^. S._Mat
    width = cols b
    height = rows b
    dense = V.create $ do
      v <- M.new (height * width)
      CM.forM_ [0..height] $ \y -> CM.forM_ [0..width] $ \x -> write v (x + y*width) (b @@> (x,y))
      return v
    resV = V.modify mutableAdd dense
    mutableAdd mv = (\action -> H.foldM action () sparse) $ \_ (S.Key r c, val) -> do
      let rr = fromIntegral r
      let cc = fromIntegral c
      vals <- M.read mv (cc + rr * width)
      write mv (cc + rr * width) (val + vals)
      return ()

sadd :: (S.Eq0 a, Num a, Container Matrix a) => CMat a -> CMat a -> CMat a
sadd Zero r = r
sadd l Zero = l
sadd (Sparse left) (Sparse right) =
    Sparse $ left + right
sadd (Dense left) (Dense right) =
    Dense $ left `add` right
sadd (Sparse left) (Dense right) =
    Dense $ sdAdd left right
sadd (Dense right) (Sparse left) =
    Dense $ sdAdd left right


smult :: (S.Eq0 a, Num a, Product a, Container Matrix a) => CMat a -> CMat a -> CMat a
smult Zero _ = Zero
smult _ Zero = Zero
smult (Sparse left) (Sparse right) =
  Sparse $ left * right
smult (Dense left) (Dense right) =
  Dense $ left `multiply` right
smult (Sparse left) (Dense right) =
  Sparse $ sdMult left right
smult (Dense left) (Sparse right) =
  Sparse $ (flip sdMult) left right

fromSparse :: [a] -> DMat a
fromSparse = undefined

requestMatrix = undefined

dadd :: (Num a, S.Eq0 a, Storable a, Container Matrix a) => DMat a -> DMat a -> Distribute (DMatMessage a) (DMat a)
dadd (Concrete cmat1) (Concrete cmat2) =
    return $ Concrete $ sadd cmat1 cmat2
dadd (Remote pid) mat = do
    rmat <- requestMatrix
    dadd rmat mat
dadd mat (Remote pid) = do
    rmat <- requestMatrix
    dadd mat rmat
dadd (DMat m1 l1 l2 l3 l4) (DMat m2 r1 r2 r3 r4) = do
  let mask = m1 .&. m2
  tl <- ternary (firstQ mask) (return l1) (dadd l1 r1)
  tr <- ternary (secondQ mask) (return l2) (dadd l2 r2)
  bl <- ternary (thirdQ mask) (return l3) (dadd l3 r3)
  br <- ternary (fourthQ mask) (return l4) (dadd l4 r4)
  return $ DMat mask tl tr bl br

dmult :: (Num a, S.Eq0 a, Storable a, Container Matrix a, Product a) =>
      DMat a -> DMat a -> Distribute (DMatMessage a) (DMat a)
dmult (Concrete cmat1) (Concrete cmat2) =
    return $ Concrete $ smult cmat1 cmat2
dmult (Remote pid) mat = do
    rmat <- requestMatrix
    dmult rmat mat
dmult mat (Remote pid) = do
    rmat <- requestMatrix
    dmult mat rmat
dmult (DMat m1 l1 l2 l3 l4) (DMat m2 r1 r2 r3 r4) = do
    let mask = m1 .&. m2
    topLeft <- ternary (firstQ mask) (return l1) $
      CM.join $ (liftM2 dadd) (dmult l1 r1) (dmult l2 r3)
    topRight <- ternary (secondQ mask) (return l2) $
      CM.join $ (liftM2 dadd) (dmult l1 r2) (dmult l2 r4)
    bottomLeft <- ternary (thirdQ mask) (return l3) $
      CM.join $ (liftM2 dadd) (dmult l3 r1) (dmult l4 r3)
    bottomRight <- ternary (fourthQ mask) (return l4) $
      CM.join $ (liftM2 dadd) (dmult l3 r2) (dmult l4 r4)
    return $ DMat mask topLeft topRight bottomLeft bottomRight
dmult _ _ = error "undefined behavior"

ternary :: Bool -> r -> r -> r
ternary False a _ = a
ternary True  _ b = b

firstQ m = (m .&. 1) == 1
secondQ m = (m .&. 13) == 1
thirdQ m = (m .&. 11) == 1
fourthQ m = (m .&. 8) == 1
