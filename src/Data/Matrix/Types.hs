{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, OverlappingInstances, ScopedTypeVariables #-}
module Data.Matrix.Types
  ( DMat(..)
  , CMat(..)
  , DMatMessage(..)
  , MElement
  ) where

import Distribute as DT
import qualified Sparse.Matrix as S
import qualified Sparse.Matrix.Internal.Array as SI
import Data.Functor
import qualified Data.Packed as D
import qualified Data.Packed.ST as DS
import Data.Packed.Development (MatrixOrder(..))
import qualified Data.Serialize as Cereal
import qualified Data.Vector.Serialize
import Foreign.Storable
import Numeric.Container
import Control.Monad
import Control.Monad.ST
import GHC.Generics

-- | Package Matrix element constraints into a convienence class.
class (Cereal.Serialize a, Num a, S.Eq0 a, Storable a, Container Matrix a, Product a) => MElement a
instance (Cereal.Serialize a, Num a, S.Eq0 a, Storable a, Container Matrix a, Product a) => MElement a

{- instance Cereal.Serialize a => (Cereal.Serialize (S.Arr a a)) where
  get = undefined
  put = undefined -}

instance ((S.Arr a ~ array), Cereal.Serialize a)  => (Cereal.Serialize (array a)) where
    get = Cereal.get :: Cereal.Get (array a)
    put = Cereal.put :: Cereal.Putter (array a)

-- This really shouldn't type check this some hacky shit
instance (SI.Arr a ~ array, Cereal.Serialize a) => (Cereal.Serialize (S.Mat a)) where
    get = do
      size <- Cereal.get
      k1 <- Cereal.get
      k2 <- Cereal.get
      content <- Cereal.get :: Cereal.Get (array a)
      return $ S.Mat size k1 k2 content

    put (S.Mat size k1 k2 content) = do
      Cereal.put size
      Cereal.put k1
      Cereal.put k2
      Cereal.put (content :: array a)

instance (Cereal.Serialize a, D.Element a) => (Cereal.Serialize (D.Matrix a)) where
    -- can most likely get better performance here with unsafeIO and
    -- unsafeStToIO
    get = do
      r <- Cereal.get :: Cereal.Get Int
      c <- Cereal.get :: Cereal.Get Int
      values <- forM [0..r] $ (\_ ->
                  forM [0..c] $ \_ -> Cereal.get)
      return $ D.fromLists values

    -- Make sure iterations happen in the row/column major order
    put mat = runST $ do
        let r = D.rows mat
        let c = D.cols mat
        mmat <- DS.thawMatrix mat
        puts <- concat <$> forM [0..r] (\i ->
                  forM [0..c] $ \j -> do
                    value <- DS.readMatrix mmat i j
                    return $ Cereal.put value)
        DS.freezeMatrix mmat
        return $ foldl (>>) (Cereal.put r >> Cereal.put c) puts

-- sparse matrix based on peano ordering
--                      top left  top right bottom left bottom right

-- | A concrete matrix either dense, or sparse, zero
data CMat a = Dense (D.Matrix a)
            | Sparse (S.Mat a)
            | Zero
            deriving (Show, Generic)

instance (Cereal.Serialize a, D.Element a)  => (Cereal.Serialize (CMat a))

-- | A distributed matrix
data DMat a = DMat !Int !(DMat a) !(DMat a) !(DMat a) !(DMat a)
            | Remote DT.PID Int
            | Concrete (CMat a)
            deriving (Show, Generic)

instance (Cereal.Serialize a, D.Element a)  => (Cereal.Serialize (DMat a))

data Q = A | B | C | D deriving (Eq, Show, Generic)

instance Cereal.Serialize Q

data DMatMessage a = Request Int
                   | Response (DMat a)
                   | Sync
                   | Finish
                   deriving (Generic)

instance (Cereal.Serialize a, D.Element a) => (Cereal.Serialize (DMatMessage a))
