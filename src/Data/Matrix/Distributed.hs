{-# LANGUAGE FlexibleContexts #-}

module Data.Matrix.Distributed where

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
import Distribute (DMat, Distribute)
import qualified Distribute as DT

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

-- sparse matrix based on peano ordering
--                  top left   top right  bottom left bottom right
data DMat a = Node !Int !(DMat a) !(DMat a) !(DMat a) !(DMat a)
            | Remote DT.PID
            | Dense (D.Matrix a)
            | Sparse (S.Mat a)
            | Zero
            deriving (Show)

{- sadd :: (S.Eq0 a, Num a, Container Matrix a) => DMat a -> DMat a -> DMat a
sadd a b = case (a, b) of
                (Zero, r) -> r
                (l, Zero) -> l
                (Sparse left, Sparse right) -> Sparse $ left + right
                (Dense left, Dense right) -> Dense $ left `add` right
                (Node _ l1 l2 l3 l4, Node _ r1 r2 r3 r4) -> Node (sadd l1 r1)
                                                             (sadd l2 r2)
                                                             (sadd l3 r3)
                                                             (sadd l4 r4)
                (Sparse left, Dense right) -> Dense $ sdAdd left right
                (Dense right, Sparse left) -> Dense $ sdAdd left right
                otherwise -> Zero -}


{- smult :: (S.Eq0 a, Num a, Product a, Container Matrix a) => DMat a -> DMat a -> DMat a
smult a b = case (a, b) of
                (Zero, _) -> Zero
                (_, Zero) -> Zero
                (Sparse left, Sparse right) -> Sparse $ left * right
                (Dense left, Dense right) -> Dense $ left `multiply` right
                (Node l1 l2 l3 l4, Node r1 r2 r3 r4) -> Node (sadd (smult l1 r1) (smult l2 r3))
                                                             (sadd (smult l1 r2) (smult l2 r4))
                                                             (sadd (smult l3 r1) (smult l4 r3))
                                                             (sadd (smult l3 r2) (smult l4 r4))
                (Sparse left, Dense right) -> Sparse $ sdMul left right
                (Dense left, Sparse right) -> Sparse $ (flip sdMul) left right
                otherwise -> Zero -- TODO: fix -}

-- current assumption that the matrix is square is bad

fromSparse :: S.Mat a -> DMat a
fromSparse = undefined

dmult :: DMat a -> DMat a -> Distribute (DMat a) DMat a
dmult (Node m1 tl1 tr1 bl1 br1) (Node m2 tl2 tr2 bl2 br2) = do
  when ((m1 .&. m2) /= 0) error "Wrong" -- assert this
  nodeMult m1 tl1 tr1 bl1 br1 tl2 tr2 bl2 br2
dmult (Node _ _ _ _ _) _ =
  error "Internal error: fuck andrew berls"
dmult _ (Node _ _ _ _ _) =
  error "Internal error: fuck pete cruz"
dmult Zero _ =
  Zero
dmult _ Zero =
  Zero
dmult (Remote pid) mat =
    remoteMult pid mat
dmult mat (Remote pid) =
    remoteMult pid mat
dmult (Sparse smat) (Dense dmat) =
    return $ Sparse (sdMult smat dmat)
dmult (Dense dmat) (Sparse smat) =
    return $ Sparse (sdMult smat dmat)
    remoteMult pid mat
  where remoteMult pid mat = do
          requestMatrix
          rmat <- DT.readFrom pid
          dmult rmat mat

        nodeMult tl tr bl br tl' tr' bl' br' = do
          topLeft <- applyOrId (firstQ mask) tl' (dmult tl)
          topRight <- applyOrId (secondQ mask) tr' (dmult tr)
          bottomLeft <- applyOrId (thirdQ mask) bl' (dmult bl)
          bottomRight <- applyOrId (fourthQ mask) br' (dmult br)
          return (Node mask topLeft topRight bottomLeft bottomRight)

        applyOrId False a _ = a
        applyOrId True  a f = f a

        firstQ m = (m .&. 1) == 1
        secondQ m = (m .&. 13) == 1
        thirdQ m = (m .&. 11) == 1
        fourthQ m = (m .&. 8) == 1
