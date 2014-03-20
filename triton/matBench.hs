import Data.Matrix.Distributed.Types
import Data.Matrix.Distributed.Builder
import Data.Matrix.Distributed


main = do
    args <- getArgs
    compute (read (args !! 0)) procs $ do
      (id, _) <- S.get
      let n   = 4
          mat = constructMat 4 id n
      lift $ print $ show id ++ " " ++ show mat
      lift $ print $ show id ++ " " ++ show vec
      lift $ print $ show id ++ " " ++ show zer
      return ()
    threadDelay 10000000
    return ()
  where procs = [(0, "localhost", 3000), (1, "localhost", 3001), (2, "localhost", 3002), (3, "localhost", 3003)]
