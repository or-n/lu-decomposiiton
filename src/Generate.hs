module Generate where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Matrix (Matrix(..), index0)
import Modify (modify)

zeroes :: (Num a, V.Unbox a) => Int -> Matrix a
zeroes n = Matrix { size = n, values = V.replicate (n * n) 0 }

identity :: (Num a, V.Unbox a) => Int -> Matrix a
identity n = z { values = modify set_diagonal_1s (values z) } where
    z = zeroes n
    diagonal = [index0 n (i, i) | i <- [0..n-1]]
    set_diagonal_1s mv = mapM_ (\i -> MV.write mv i 1) diagonal

one :: (Num a, V.Unbox a) => Int -> Int -> V.Vector a
one n i = modify (\mv -> MV.write mv i 1) (V.replicate n 0)