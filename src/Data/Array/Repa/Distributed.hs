module Data.Array.Repa.Distributed where

import Data.Array.Repa.Distributed

distribute :: IO () -> IO ()
distribute = do
  econfig <- parseConfig "test.config"
  case econfig of
    Left e  -> error $ show e
    Right v ->
      
  
