import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Test.HUnit as H

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


main = defaultMain $ testGroup "Tests" [unitTests] -- [qcProps]

unitTests = testGroup "HUnit" [test_sync] -- [test_local_mult]

qcProps = testGroup "QuickCheck" [
    property_sparse_dense_add,
    property_sparse_dense_mult,
    property_sparse_mult ]

property_sparse_dense_add = QC.testProperty "sparse + dense" test
  where prop x y = 

property_sparse_dense_mult = QC.testProperty "sparse * dense" (undefined :: String -> Bool)

property_sparse_mult = QC.testProperty "sparse * sparse" (undefined :: String -> Bool)

{- test_local_mult = HU.testCase "multiplication of two identitty matrices should yield and identity matrix" test
  where
    test = DT.emptyRegistry >>= (\r -> ST.evalStateT (1, r)) $ do
      let mat = DMat 3 (Concrete (Dense (ident 8))) (Concrete Zero) (Concrete Zero) (Concrete (Sparse (S.ident 8)))
      let mat' = DMat 3 mat (Concrete Zero) (Concrete Zero) mat
      result <- dmult mat' mat'
      assertEqual "expecting identity" mat' result -}

test_sync = HU.testCase "sync should allow for messages to be received by all processes" test
  where test = do
          reg1 <- DT.emptyRegistry :: IO (DT.Registry (DMatMessage Double))
          reg2 <- DT.emptyRegistry :: IO (DT.Registry (DMatMessage Double))
          let mat = constructMat 10 0 4 -- right here
          let mat2 = constructMat 10 1 4 -- right here
          print mat
          print "---------------"
          print mat2
          print "---------------"
          -- Need some kind of data here
          forkIO $ (flip ST.evalStateT) (0, reg1) $ do
            st <- ST.get
            DT.start 3000 (DT.registerIncoming st)
            ST.lift $ putStrLn "above sync"
            lift $ threadDelay 1000
            sync (mat, mat) (requestMatrix 1 L [B])
            ST.lift $ putStrLn "Done1"
            return ()

          (flip ST.evalStateT) (1, reg2)  $ do
            st' <- ST.get
            DT.start 4000 (DT.registerIncoming st')
            DT.open "localhost" 3000
            ST.lift $ putStrLn "above sync"
            sync (mat2, mat2) (requestMatrix 0 L [A])
            ST.lift $ putStrLn "DOne2"
            return ()

          return ()
