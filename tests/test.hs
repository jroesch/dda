import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Test.HUnit as H

import Data.Matrix.Types
import Data.Matrix.Distributed
import qualified Data.Packed as D
import Numeric.Container
import qualified Sparse.Matrix as S
import qualified Control.Monad.State as ST
import qualified Distribute as DT
import Control.Lens

main = defaultMain $ testGroup "Tests" [qcProps]

unitTests = testGroup "HUnit" [test_local_mult]

qcProps = testGroup "QuickCheck" [
    property_sparse_dense_add,
    property_sparse_dense_mult,
    property_sparse_mult]

property_sparse_dense_add = QC.testProperty "sparse + dense" (undefined :: String -> Bool)

property_sparse_dense_mult = QC.testProperty "sparse * dense" (undefined :: String -> Bool)

property_sparse_mult = QC.testProperty "sparse * sparse" (undefined :: String -> Bool)

test_local_mult = HU.testCase "multiplication of two identitty matrices should yield and identity matrix" test
  where
    test = do
      let mat = DMat 3 (Concrete (Dense (ident 8))) (Concrete Zero) (Concrete Zero) (Concrete (Sparse (S.ident 8)))
      let mat' = DMat 3 mat (Concrete Zero) (Concrete Zero) mat
      let result = dmult mat' mat'
      assertEqual "expecting identity" mat' result

test_sync = HU.testCase "sync should allow for messages to be received by all processes" test
  where test = DT.emptyRegistry (\r -> evalStateT (1, r)) $ do
          DT.start 3000 DT.registerIncoming
          st <- ST.get
          sync $ do
            mat <- requestMatrix 2 2
          DT.localState (set _1 2 st) $ sync $ do
            mat <- requestMatrix 1 1
          return ()
            


