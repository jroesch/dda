module Data.Matrix.Distributed.Builder where

import Data.Matrix.Distributed
import Data.Matrix.Distributed.Types
import qualified Sparse.Matrix as S
import Data.Bits

constructMat :: MElement a => Int -> Int -> Int -> DMat a
constructMat s id n = constructMat' (fromIntegral s) (fromIntegral id) (fromIntegral n) 0

-- construct a matrix of size s x s
constructMat'' s id n off = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete $ Sparse $ S.fromList list) (Remote (off + 1) [B]) (Remote (off' + 2) [C]) (Remote (off' + 3) [D])
            1 -> DMat 2 (Remote (off' + 0) [A]) (Concrete $ Sparse $ S.fromList list) (Remote (off' + 2) [C]) (Remote (off' + 3) [D])
            2 -> DMat 4 (Remote (off' + 0) [A]) (Remote (off' + 1) [B]) (Concrete $ Sparse $ S.fromList list) (Remote (off' + 3) [D])
            3 -> DMat 8 (Remote (off' + 0) [A]) (Remote (off' + 1) [B]) (Remote (off' + 2) [C]) (Concrete $ Sparse $ S.fromList list)
    off' = fromIntegral off
    n' = floor $ sqrt $ fromIntegral n
    ix = id `mod` n'
    iy = id `div` n'
    list = filter (\(S.Key x y, v) -> x >= ix * s `div` n' && x < (ix+1) * s `div` n' && y >= iy * s `div`n' && y < (iy+1) * s `div` n') xs
    s' = floor $ sqrt $ fromIntegral s
    xs = go 0 []
    go x xs | x >= s = xs
    go x xs = go (x+1) $ top x $ right x $ bot x $ left x $ ((S.Key x x, 4):xs)
    left x xs = if x `mod` s' < 1 then xs else (S.Key (x-1) x, -1):xs
    bot  x xs = if x `div` s' >= s'-1 then xs else (S.Key (x+s') x, -1):xs
    top  x xs = if x `div` s' < 1 then xs else (S.Key (x-s') x, -1):xs
    right x xs = if x `mod` s' >= s'-1 then xs else (S.Key (x+1) x, -1):xs

constructMat' s id n off = case n of 
                            4 | id >= off && id < off + n' -> constructMat'' s (id-(fromIntegral off)) n (fromIntegral off)
                            4 -> (Remote ((fromIntegral off) + (fromIntegral id)) quad)
                            _ -> DMat 15 (constructMat' s id n' 0)
                                         (constructMat' s id n' n')
                                         (constructMat' s id n' (2*n'))
                                         (constructMat' s id n' (3*n'))
                            
  where
    n' = n `div` 4
    quad = case id of
             0 -> [A]
             1 -> [B]
             2 -> [C]
             3 -> [D]

{-
constructMat s id n l off = case n of
                        1 -> Concrete $ Sparse $ S.fromList $ filter f l
                        2 -> undefined
                        4 -> DMat (0 shift (id `mod` n)) (constructMat (s `div` 2) id (n `div` 4) l
                        x -> DMat (
  where
    i = id `mod` 4
    ix = i `mod` n
    iy = i `div` n
    f (S.Key x y, v) = 

constructMat' s = go 0 []
  where
    s' = floor $ sqrt $ fromIntegral s
    go x xs | x >= s = xs
    go x xs = go (x+1) $ top x $ right x $ bot x $ left x $ ((S.Key x x, 4):xs)
    left x xs = if x `mod` s' < 1 then xs else (S.Key (x-1) x, -1):xs
    bot  x xs = if x `div` s' >= s'-1 then xs else (S.Key (x+s') x, -1):xs
    top  x xs = if x `div` s' < 1 then xs else (S.Key (x-s') x, -1):xs
    right x xs = if x `mod` s' >= s'-1 then xs else (S.Key (x+1) x, -1):xs
-}

constructVec s id n = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete $ Sparse $ S.fromList $ go 0 (s `div` 2) []) (Remote 1 [B]) (Remote 2 [C]) (Remote 3 [D])
            1 -> DMat 2 (Remote 0 [A]) (Concrete Zero) (Remote 2 [C]) (Remote 3 [D]) 
            2 -> DMat 4 (Remote 0 [A]) (Remote 1 [B]) (Concrete Zero) (Remote 3 [D])
            3 -> DMat 8 (Remote 0 [A]) (Remote 1 [B]) (Remote 2 [C]) (Concrete $ Sparse $ S.fromList $ go 0 (s `div` 2) [])
    go x e xs | x >= e = xs
    go x e xs = go (x+1) e ((S.Key 0 x, 1):xs)

zeros s id n = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete Zero) (Remote 1 [B]) (Remote 2 [C]) (Remote 3 [D])
            1 -> DMat 2 (Remote 0 [A]) (Concrete Zero) (Remote 2 [C]) (Remote 3 [D])
            2 -> DMat 4 (Remote 0 [A]) (Remote 1 [B]) (Concrete Zero) (Remote 3 [D])
            3 -> DMat 8 (Remote 0 [A]) (Remote 1 [B]) (Remote 2 [C]) (Concrete Zero)
