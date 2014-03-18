module Instances () where

import Control.Applicative
import Data.Matrix.Distributed
import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Sync
import Data.Matrix.Distributed.Builder

import qualified Data.Packed as D
import Numeric.Container
import qualified Sparse.Matrix as S
import qualified Control.Monad.State as ST
import qualified Distribute as DT
import Control.Lens
import qualified Sparse.Matrix
import Data.Packed
import Control.Monad.Trans (lift)
import Control.Concurrent
import Test.QuickCheck.Arbitrary
import qualified Data.Map as M

instance Arbitrary S.Key where
  arbitrary = S.Key <$> arbitrary <*> arbitrary

instance (S.Arrayed a, Arbitrary a) => Arbitrary (S.Mat a) where
  arbitrary = S.fromList . M.toList <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary
