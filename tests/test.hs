import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Test.HUnit as H

main = defaultMain $ testGroup "Tests" [qcProps]

-- unitTests = testGroup "HUnit" [test_open]

qcProps = testGroup "QuickCheck" [
    property_sparse_dense_add, 
    property_sparse_dense_mult, 
    property_sparse_mult]

property_sparse_dense_add = QC.testProperty "sparse + dense" (undefined :: String -> Bool)

property_sparse_dense_mult = QC.testProperty "sparse * dense" (undefined :: String -> Bool)

property_sparse_mult = QC.testProperty "sparse * sparse" (undefined :: String -> Bool)
