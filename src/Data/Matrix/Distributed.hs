{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, BangPatterns #-}
module Data.Matrix.Distributed where

import Control.Applicative
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Sync
import Data.Word
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Hybrid as H
import qualified Sparse.Matrix as S
import qualified Data.Packed as D
import Foreign.Storable
import Numeric.Container
import Control.Monad as CM
import Control.Lens
import Distribute (Distribute)
import qualified Distribute as DT
import qualified Data.Serialize as Cereal
import qualified Control.Monad.State as S
import Control.Monad.Trans (lift)
import Control.Concurrent

compute :: (MElement a) => DT.PID -> [(DT.PID, String, Int)] -> Distribute (DMatMessage a) () -> IO ()
compute pid procs action = do
    a <- newMVar ()
    forM_ procs $ \(pid1, host1, port1) -> do
      when (pid1 == pid) $ do
        putStrLn $ show pid1 ++ " starting up on " ++ show host1 ++ ":" ++ show port1
        putStrLn $ "At registry with host: " ++ (host1)
        reg <- DT.emptyRegistry
        let state = (pid1, reg)
        putStrLn "At thread start"
        DT.runDistribute state $ do
          DT.start port1 (DT.registerIncoming state)
          lift $ threadDelay 10000
          forM_ procs $ \(pid2, host2, port2) -> do
            lift $ print (pid1, pid2)
            when (pid2 < pid1) $ do
              lift $ print $ (show pid1) ++ " trying to connect to " ++ (show pid2) ++ "  " ++ show host2 ++ ":" ++ show port2
              DT.open host2 port2
              lift $ print $ show pid1 ++ " connected to " ++ show pid2
              return ()
            return ()
          lift $ putStrLn $ show pid ++ " is setup"
          lift $ putStrLn "Running Action"
          action
          lift $ putStrLn "Done Action"
          return ()
        return ()

startProcess = undefined

topleft :: (MElement a) => DMat a -> Distribute (DMatMessage a) a
topleft !m = sync (m, m) (unsafeTopLeft m)
  where
    unsafeTopLeft :: (MElement a) => DMat a -> Requests a a
    unsafeTopLeft (Concrete (Dense smat)) = return $ smat @@> (0,0)
    unsafeTopLeft (Concrete (Sparse smat)) = let list = H.filter (\(S.Key a b, _) -> a == 0 && b == 0) $ (from S._Mat) # smat
                                              in return $ if H.null list then 0 else snd $ list H.! 0
    unsafeTopLeft (Concrete Zero) = return 0
    unsafeTopLeft r @ (Remote pid quad) = do
        !mat <- requestMatrix pid L quad
        unsafeTopLeft mat
    unsafeTopLeft (DMat _ smat _ _ _) = unsafeTopLeft smat

opOnNonZeros :: (MElement a) => (a -> a) -> DMat a -> DMat a
opOnNonZeros op (Concrete (Dense smat)) = Concrete $ Dense $ D.mapMatrix op smat
opOnNonZeros op (Concrete (Sparse smat)) =
  Concrete $ Sparse $ (smat & S._Mat %~ H.map (\(k, b) -> (k, op b)))
opOnNonZeros op (Concrete Zero) = Concrete Zero
opOnNonZeros op (Remote pid quad) = Remote pid quad
opOnNonZeros op (DMat mask tl tr bl br) =
    DMat mask (opOnNonZeros op tl) (opOnNonZeros op tr)
              (opOnNonZeros op bl) (opOnNonZeros op br)

{-# RULES
    "opOnNonZeros/opOnNonZeros" forall f g xs. opOnNonZeros f (opOnNonZeros g xs) = opOnNonZeros (f.g) xs
  #-}

infixl 7 *#
(*#) :: MElement a => a -> DMat a -> DMat a
a *# b = opOnNonZeros (a *) b

infixl 7 #*
(#*) :: MElement a => DMat a -> a -> DMat a
a #* b = opOnNonZeros (b *) a

transpose :: (S.Arrayed a) => DMat a -> DMat a
transpose (Concrete (Dense smat)) = Concrete $ Dense $ D.trans smat
transpose (Concrete (Sparse smat)) = Concrete $ Sparse $ S.transpose smat
transpose (Concrete Zero) = Concrete Zero
transpose (Remote pid quad) = case quad of
                                [B] -> Remote pid [C]
                                [C] -> Remote pid [B]
                                a -> Remote pid a
transpose (DMat mask tl tr bl br) =
    DMat (mask' mask) (transpose tl) (transpose bl)
                      (transpose tr) (transpose br)
  where
    mask' m = (m .&. 1) + (m .&. 8) + (if thirdQ m then 1 else 0)*2 + (if secondQ m then 1 else 0)*4

{-# RULES
    "transpose/transpose" forall a. transpose (transpose a) = a
  #-}

-- sparse-dense elementwise multiply zero * somehting = zero
-- resulting array is sparse
sdMult :: MElement a => (a -> a -> a) -> S.Mat a -> D.Matrix a -> S.Mat a
sdMult op a b = a & S._Mat %~ H.map (\(S.Key r c, d) -> (S.Key r c, d `op` (b @@> (fromIntegral r, fromIntegral c))))

-- TODO: am i accidentaly transposing
-- sparse-desnse add yields dense (we assume not too many zeros are created)
sdAdd :: MElement a => (a -> a -> a) -> S.Mat a -> D.Matrix a -> D.Matrix a
sdAdd op a b = (height><width) $ V.toList resV
  where
    sparse = a ^. S._Mat
    width = cols b
    height = rows b
    dense = V.create $ do
      v <- M.new (height * width)
      CM.forM_ [0..height] $ \y -> CM.forM_ [0..width] $ \x -> M.write v (x + y*width) (b @@> (x,y))
      return v
    resV = V.modify mutableAdd dense
    mutableAdd mv = (\action -> H.foldM action () sparse) $ \_ (S.Key r c, val) -> do
      let rr = fromIntegral r
      let cc = fromIntegral c
      vals <- M.read mv (cc + rr * width)
      M.write mv (cc + rr * width) (val `op` vals)
      return ()

sadd :: MElement a => CMat a -> CMat a -> CMat a
sadd Zero r = r
sadd l Zero = l
sadd (Sparse left) (Sparse right) =
    Sparse $ left + right
sadd (Dense left) (Dense right) =
    Dense $ left `add` right
sadd (Sparse left) (Dense right) =
    Dense $ sdAdd (+) left right
sadd (Dense right) (Sparse left) =
    Dense $ sdAdd (+) left right

smult :: MElement a => CMat a -> CMat a -> CMat a
smult Zero _ = Zero
smult _ Zero = Zero
smult (Sparse left) (Sparse right) =
  Sparse $ left * right
smult (Dense left) (Dense right) =
  Dense $ left `multiply` right
smult (Sparse left) (Dense right) =
  Sparse $ sdMult (*) left right
smult (Dense left) (Sparse right) =
  Sparse $ (flip (sdMult (*))) left right

fromSparse :: [a] -> DMat a
fromSparse = undefined

{-
data Semaphore = Semaphore (MVar ()) (MVar Int) Int

up :: Int -> Semaphore -> IO ()
up delta (Semaphore mvar mcount total) = do
    count <- takeMVar mcount
    if count + delta >= total
      then do

        putMVar mvar ()

      else putMVar mcount (count + delta)

up :: Int -> Semaphore -> IO ()
up count' (Semaphore mvar count total)
  | count + count' <= total =
  | otherwise = -}

{- down :: Int -> Semaphore -> IO ()
down count' (Semaphore mvar mcount total) = do
  count >= total = putMVar mvar ()
  | otherwise

down :: Int -> Semaphore -> IO ()
down delta (Semaphore mvar mcount total) = do
    count <- takeMVar mcount -}


dadd :: MElement a => DMat a -> DMat a -> Distribute (DMatMessage a) (DMat a)
dadd r l = sync (l, r) $ (dadd' l r)
 
dadd' (Concrete cmat1) (Concrete cmat2) =
    return $ Concrete $ sadd cmat1 cmat2
dadd' (Remote pid quad) (Remote pid' quad') =
    return $ Remote pid quad
dadd' l @ (Remote pid quad) mat = do
    !rmat <- requestMatrix pid R quad
    dadd' rmat mat
dadd' mat r @ (Remote pid quad) = do
    !rmat <- requestMatrix pid L quad
    dadd' mat rmat
dadd' (DMat m1 l1 l2 l3 l4) (DMat m2 r1 r2 r3 r4) = do
    let mask = m1 .&. m2
    tl <- ternary (firstQ mask) (return l1) (dadd' l1 r1)
    tr <- ternary (secondQ mask) (return l2) (dadd' l2 r2)
    bl <- ternary (thirdQ mask) (return l3) (dadd' l3 r3)
    br <- ternary (fourthQ mask) (return l4) (dadd' l4 r4)
    return $ DMat mask tl tr bl br

-- elementwise operations

-- elementwise add on semiring, can result in zeros
eadd :: (MElement a, Storable (a, a)) => (a -> a -> a) -> CMat a -> CMat a -> CMat a
eadd op (Sparse sa) (Sparse sb) =
    Sparse $ S.addWith0 (S.nonZero op) sa sb
eadd op (Dense da) (Dense db) =
    Dense $ D.liftMatrix2 (\v1 v2 -> mapVector (\(a1, a2) -> op a1 a2) (zipVector v1 v2)) da db
eadd op (Dense da) (Sparse sb) =
    Dense $ sdAdd op sb da
eadd op (Sparse sa) (Dense db) =
  Dense $ sdAdd op sa db
eadd op Zero other = other
eadd op other Zero = other

edadd :: (MElement a,  Storable (a, a)) => (a -> a -> a) -> DMat a -> DMat a -> DMat a
edadd op (Concrete a) (Concrete b) = Concrete $ eadd op a b
edadd op (Remote pid1 a) (Remote pid2 b) {- | pid1 == pid2 -} = Remote pid1 a
edadd op (DMat mask a1 a2 a3 a4) (DMat mask' b1 b2 b3 b4) =
    DMat mask (edadd op a1 b1) (edadd op a2 b2)
              (edadd op a3 b3) (edadd op a4 b4)

infixl 6 ^+
(^+) :: (MElement a,  Storable (a, a)) => DMat a -> DMat a -> DMat a
(^+) =  edadd (+)
infixl 6 ^-
(^-) :: (MElement a,  Storable (a, a)) => DMat a -> DMat a -> DMat a
(^-) =  edadd (-)

-- elementwise multiply on semiring, multiplication by zero = zero
emult :: (MElement a, Storable (a, a)) => (a -> a -> a) -> CMat a -> CMat a -> CMat a
emult op a b = case (a, b) of
                 (Sparse sa, Sparse sb) -> Sparse $ S.elementMultiplyWith op sa sb
                 (Dense da,  Dense db)  -> Dense $ D.liftMatrix2 (\v1 v2 -> mapVector (\(a1, a2) -> op a1 a2) (zipVector v1 v2)) da db
                 (Dense da,  Sparse sb) -> Sparse $ sdMult op sb da
                 (Sparse sa, Dense db)  -> Sparse $ sdMult op sa db
                 (Zero,      other)     -> other
                 (other,     Zero)      -> other

(^*) :: (MElement a, Storable (a, a)) => DMat a -> DMat a -> DMat a
(^*) = edmult (*)

edmult :: (MElement a, Storable (a, a)) => (a -> a -> a) -> DMat a -> DMat a -> DMat a
edmult op (Concrete a) (Concrete b) = Concrete $ emult op a b
edmult op (Remote pid1 a) (Remote pid2 b) = Remote pid1 a
edmult op (DMat mask a1 a2 a3 a4) (DMat mask' b1 b2 b3 b4) =
    DMat mask (edmult op a1 b1) (edmult op a2 b2)
              (edmult op a3 b3) (edmult op a4 b4)
infixr 7 .*
(.*) = dmult

dmult :: (MElement a) => DMat a -> DMat a -> Distribute (DMatMessage a) (DMat a)
dmult !l !r = sync (l, r) (dmult' l r)
  where 
    dmult' (Concrete cmat1) (Concrete cmat2) =
        return $ Concrete $ smult cmat1 cmat2
    dmult' l @ (Remote pid quad) mat = do
        !lmat <- requestMatrix pid L quad
        dmult' lmat mat
    dmult' mat r @ (Remote pid quad) = do
        !rmat <- requestMatrix pid R quad
        dmult' mat rmat
    dmult' (DMat m1 l1 l2 l3 l4) (DMat m2 r1 r2 r3 r4) = do
        let mask = m1 .&. m2
        topLeft <- ternary (firstQ mask) (return l1) $
          CM.join $ (liftM2 dadd') (dmult' l1 r1) (dmult' l2 r3)
        topRight <- ternary (secondQ mask) (return l2) $
          CM.join $ (liftM2 dadd') (dmult' l1 r2) (dmult' l2 r4)
        bottomLeft <- ternary (thirdQ mask) (return l3) $
          CM.join $ (liftM2 dadd') (dmult' l3 r1) (dmult' l4 r3)
        bottomRight <- ternary (fourthQ mask) (return l4) $
          CM.join $ (liftM2 dadd') (dmult' l3 r2) (dmult' l4 r4)
        return $ DMat mask topLeft topRight bottomLeft bottomRight
    dmult' _ _ = error "undefined behavior"

ternary :: Bool -> r -> r -> r
ternary False a _ = a
ternary True  _ b = b

firstQ m = (m .&. 1) == 1
secondQ m = (m .&. 2) == 2
thirdQ m = (m .&. 4) == 4
fourthQ m = (m .&. 8) == 8
