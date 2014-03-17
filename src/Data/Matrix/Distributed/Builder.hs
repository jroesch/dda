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
                            4 | id >= off && id < off + n -> constructMat'' s (id-(fromIntegral off)) n (fromIntegral off)
                            4 -> DMat 15 (Remote ((fromIntegral off) + (fromIntegral id)) [A])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 1) [B])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 2) [C])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 3) [D])
                            _ -> DMat 15 (constructMat' s id n' 0)
                                         (constructMat' s id n' n')
                                         (constructMat' s id n' (2*n'))
                                         (constructMat' s id n' (3*n'))
                            
  where
    n' = n `div` 4

constructVec :: MElement a => Int -> Int -> Int -> DMat a
constructVec s id n = zeros' s id n 0

constructVec' s id n off = case n of 
                            4 | id >= off && id < off + n' -> constructVec'' s (id-(fromIntegral off)) n (fromIntegral off)
                            4 -> DMat 15 (Remote ((fromIntegral off) + (fromIntegral id)) [A])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 1) [B])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 2) [C])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 3) [D])
                            _ -> DMat 15 (constructVec' s id n' 0)
                                         (constructVec' s id n' n')
                                         (constructVec' s id n' (2*n'))
                                         (constructVec' s id n' (3*n'))
  where
    n' = n `div` 4

constructVec'' s id n off = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete $ Sparse $ S.fromList $ go 0 (s `div` 2) []) (Remote (off + 1) [B]) (Remote (off + 2) [C]) (Remote (off + 3) [D])
            1 -> DMat 2 (Remote (off + 0) [A]) (Concrete Zero) (Remote (off + 2) [C]) (Remote (off + 3) [D]) 
            2 -> DMat 4 (Remote (off + 0) [A]) (Remote (off + 1) [B]) (Concrete Zero) (Remote (off + 3) [D])
            3 -> DMat 8 (Remote (off + 0) [A]) (Remote (off + 1) [B]) (Remote (off + 2) [C]) (Concrete $ Sparse $ S.fromList $ go 0 (s `div` 2) [])
    go x e xs | x >= e = xs
    go x e xs = go (x+1) e ((S.Key 0 x, 1):xs)

zeros :: MElement a => Int -> Int -> Int -> DMat a
zeros s id n = zeros' s id n 0

zeros' s id n off = case n of 
                            4 | id >= off && id < off + n' -> zeros'' s (id-(fromIntegral off)) n (fromIntegral off)
                            4 -> DMat 15 (Remote ((fromIntegral off) + (fromIntegral id)) [A])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 1) [B])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 2) [C])
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 3) [D])
                            _ -> DMat 15 (zeros' s id n' 0)
                                         (zeros' s id n' n')
                                         (zeros' s id n' (2*n'))
                                         (zeros' s id n' (3*n'))
  where
    n' = n `div` 4

zeros'' s id n off = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete Zero) (Remote (off + 1) [B]) (Remote (off + 2) [C]) (Remote (off + 3) [D])
            1 -> DMat 2 (Remote (off + 0) [A]) (Concrete Zero) (Remote (off + 2) [C]) (Remote (off + 3) [D])
            2 -> DMat 4 (Remote (off + 0) [A]) (Remote (off + 1) [B]) (Concrete Zero) (Remote (off + 3) [D])
            3 -> DMat 8 (Remote (off + 0) [A]) (Remote (off + 1) [B]) (Remote (off + 2) [C]) (Concrete Zero)
