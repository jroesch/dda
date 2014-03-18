{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, OverlappingInstances #-}
module Data.Matrix.Distributed.Types
  ( DMat(..)
  , CMat(..)
  , Arg(..)
  , Q(..)
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
import qualified Data.Vector.Hybrid as H
import qualified Data.Vector.Hybrid.Internal as H
import Foreign.Storable
import Numeric.Container
import Control.Monad
import Control.Monad.ST
import GHC.Generics
import Control.Lens as L

import Data.ByteString (pack, unpack)

-- Would be nice to have clean type alias for this!
type DMatrix a r = Distribute (DMatMessage a)

-- | Package Matrix element constraints into a convienence class.
class ( SI.Arrayed a
      , Cereal.Serialize a
      , Num a, S.Eq0 a
      , Storable a
      , Container Vector a
      , Container Matrix a
      , Product a
      , Show a
      ) => MElement a

instance ( SI.Arrayed a
         , Cereal.Serialize a
         , Num a, S.Eq0 a
         , Storable a
         , Container Vector a
         , Container Matrix a
         , Product a
         , Show a
         ) =>  MElement a

instance Cereal.Serialize S.Key where
    get = do
      w1 <- Cereal.get
      w2 <- Cereal.get
      return $ S.Key w1 w2

    put (S.Key w1 w2) = do
      Cereal.put w1
      Cereal.put w2

instance (SI.Arrayed a, Cereal.Serialize a) => (Cereal.Serialize (S.Mat a)) where
    get = (\x -> S._Mat # H.fromList x) `fmap` Cereal.get
    put mat = Cereal.put $ H.toList $ L.from S._Mat # mat

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

-- | A concrete matrix either dense, or sparse, zero
data CMat a = Dense (D.Matrix a)
            | Sparse (S.Mat a)
            | Zero
            deriving (Show, Generic)

instance (SI.Arrayed a, Cereal.Serialize a, D.Element a)  => (Cereal.Serialize (CMat a))

-- | A distributed matrix representing a quad tree, a remote piece or a local piece.
data DMat a = DMat !Int !(DMat a) !(DMat a) !(DMat a) !(DMat a)
            | Remote !DT.PID ![Q]
            | Concrete !(CMat a)
            deriving (Show, Generic)

instance (SI.Arrayed a, Cereal.Serialize a, D.Element a)  => (Cereal.Serialize (DMat a))

data Q = A | B | C | D deriving (Eq, Show, Generic)

instance Cereal.Serialize Q

data Arg = L | R deriving (Eq, Show, Generic)

instance Cereal.Serialize Arg

data DMatMessage a = Request !Arg ![Q]
                   | Response !(CMat a)
                   | Sync
                   | Finish
                   deriving (Show, Generic)

instance (SI.Arrayed a, Cereal.Serialize a, D.Element a) => (Cereal.Serialize (DMatMessage a))
