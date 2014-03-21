module Data.Matrix.Distributed.Builder where

import Data.Matrix.Distributed
import Data.Matrix.Distributed.Types
import qualified Sparse.Matrix as S
import Data.Bits

constructMat :: MElement a => Int -> Int -> Int -> DMat a
constructMat s id n = constructMat' (fromIntegral s) (fromIntegral id) (fromIntegral n) 0 []

-- construct a matrix of size s x s
constructMat'' s id n off side = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete $ Sparse $ S.fromList list) (Remote (off + 1) b_) (Remote (off' + 2) c_) (Remote (off' + 3) d_)
            1 -> DMat 2 (Remote (off' + 0) a_) (Concrete $ Sparse $ S.fromList list) (Remote (off' + 2) c_) (Remote (off' + 3) d_)
            2 -> DMat 4 (Remote (off' + 0) a_) (Remote (off' + 1) b_) (Concrete $ Sparse $ S.fromList list) (Remote (off' + 3) d_)
            3 -> DMat 8 (Remote (off' + 0) a_) (Remote (off' + 1) b_) (Remote (off' + 2) c_) (Concrete $ Sparse $ S.fromList list)
    a_ = side ++ [A]
    b_ = side ++ [B]
    c_ = side ++ [C]
    d_ = side ++ [D]
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

constructMat' s id n off side = case n of 
                            4 | id >= off && id < off + n -> constructMat'' s (id-(fromIntegral off)) n (fromIntegral off) side
                            4 -> DMat 15 (Remote (fromIntegral off) (side ++ [A]))
                                         (Remote ((fromIntegral off) + 1) (side ++ [B]))
                                         (Remote ((fromIntegral off) + 2) (side ++ [C]))
                                         (Remote ((fromIntegral off) + 3) (side ++ [D]))
                            _ -> DMat 15 (constructMat' s id n' (off) (side ++ [A]))
                                         (constructMat' s id n' (off + n') (side ++ [B]))
                                         (constructMat' s id n' (off + 2*n') (side ++ [C]))
                                         (constructMat' s id n' (off + 3*n') (side ++ [D]))
  where
    n' = n `div` 4

constructIdent :: MElement a => Int -> Int -> Int -> DMat a
constructIdent s id n = constructMat' (fromIntegral s) (fromIntegral id) (fromIntegral n) 0 []

-- construct a matrix of size s x s
constructIdent'' s id n off side = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete $ Sparse $ S.ident s) (Remote (off' + 1) b_) (Remote (off' + 2) c_) (Remote (off' + 3) d_)
            1 -> DMat 2 (Remote (off' + 0) a_) (Concrete Zero) (Remote (off' + 2) c_) (Remote (off' + 3) d_)
            2 -> DMat 4 (Remote (off' + 0) a_) (Remote (off' + 1) b_) (Concrete Zero) (Remote (off' + 3) d_)
            3 -> DMat 8 (Remote (off' + 0) a_) (Remote (off' + 1) b_) (Remote (off' + 2) c_) (Concrete $ Sparse $ S.ident s)
    a_ = side ++ [A]
    b_ = side ++ [B]
    c_ = side ++ [C]
    d_ = side ++ [D]
    off' = fromIntegral off
    n' = floor $ sqrt $ fromIntegral n

constructIdent' s id n off side = case n of 
                            4 | id >= off && id < off + n -> constructIdent'' s (id-(fromIntegral off)) n (fromIntegral off) side
                            4 -> DMat 15 (Remote (fromIntegral off) (side ++ [A]))
                                         (Remote ((fromIntegral off) + 1) (side ++ [B]))
                                         (Remote ((fromIntegral off) + 2) (side ++ [C]))
                                         (Remote ((fromIntegral off) + 3) (side ++ [D]))
                            _ -> DMat 15 (constructIdent' s id n' (off) (side ++ [A]))
                                         (constructIdent' s id n' (off + n') (side ++ [B]))
                                         (constructIdent' s id n' (off + 2*n') (side ++ [C]))
                                         (constructIdent' s id n' (off + 3*n') (side ++ [D]))
  where
    n' = n `div` 4

constructVec :: MElement a => Int -> Int -> Int -> DMat a
constructVec s id n = constructVec' (fromIntegral s) id n 0 []

constructVec' s id n off side = case n of 
                            4 | id >= off && id < off + n -> constructVec'' s (id-(fromIntegral off)) n (fromIntegral off) side
                            4 -> DMat 15 (Remote ((fromIntegral off) + (fromIntegral id)) (side ++ [A]))
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 1) (side ++ [B]))
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 2) (side ++ [C]))
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 3) (side ++ [D]))
                            _ -> DMat 15 (constructVec' s id n' 0 (side ++ [A]))
                                         (constructVec' s id n' n' (side ++ [B]))
                                         (constructVec' s id n' (2*n') (side ++ [C]))
                                         (constructVec' s id n' (3*n') (side ++ [D]))
  where
    n' = n `div` 4

constructVec'' s id n off side = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete $ Sparse $ S.fromList $ go 0 s []) (Remote (off + 1) b_) (Remote (off + 2) c_) (Remote (off + 3) d_)
            1 -> DMat 2 (Remote (off + 0) a_) (Concrete Zero) (Remote (off + 2) c_) (Remote (off + 3) d_) 
            2 -> DMat 4 (Remote (off + 0) a_) (Remote (off + 1) b_) (Concrete $ Sparse $ S.fromList $ go 0 s []) (Remote (off + 3) d_)
            3 -> DMat 8 (Remote (off + 0) a_) (Remote (off + 1) b_) (Remote (off + 2) c_) (Concrete Zero)
    a_ = side ++ [A]
    b_ = side ++ [B]
    c_ = side ++ [C]
    d_ = side ++ [D]
    go x e xs | x >= e = xs
    go x e xs = go (x+1) e ((S.Key 0 x, 1):xs)

zeros :: MElement a => Int -> Int -> Int -> DMat a
zeros s id n = zeros' s id n 0 []

zeros' s id n off side = case n of 
                            4 | id >= off && id < off + n -> zeros'' s (id-(fromIntegral off)) n (fromIntegral off) side
                            4 -> DMat 15 (Remote ((fromIntegral off) + (fromIntegral id)) (side ++ [A]))
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 1) (side ++ [B]))
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 2) (side ++ [C]))
                                         (Remote ((fromIntegral off) + (fromIntegral id) + 3) (side ++ [D]))
                            _ -> DMat 15 (zeros' s id n' 0 (side ++ [A]))
                                         (zeros' s id n' n' (side ++ [B]))
                                         (zeros' s id n' (2*n') (side ++ [C]))
                                         (zeros' s id n' (3*n') (side ++ [D]))
  where
    n' = n `div` 4

zeros'' s id n off side = mat
  where
    mat = case id of
            0 -> DMat 1 (Concrete Zero) (Remote (off + 1) b_) (Remote (off + 2) c_) (Remote (off + 3) d_)
            1 -> DMat 2 (Remote (off + 0) a_) (Concrete Zero) (Remote (off + 2) c_) (Remote (off + 3) d_)
            2 -> DMat 4 (Remote (off + 0) a_) (Remote (off + 1) b_) (Concrete Zero) (Remote (off + 3) d_)
            3 -> DMat 8 (Remote (off + 0) a_) (Remote (off + 1) b_) (Remote (off + 2) c_) (Concrete Zero)
    a_ = side ++ [A]
    b_ = side ++ [B]
    c_ = side ++ [C]
    d_ = side ++ [D]
