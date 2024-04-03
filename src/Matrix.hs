module Matrix where

import qualified Data.Vector.Unboxed as V

data Matrix a = Matrix
    { size :: Int
    , values :: V.Vector a
    }

instance (Show a, V.Unbox a) => Show (Matrix a) where
    show matrix = unlines (map (unwords . map show) rows) where
        n = size matrix
        v = values matrix
        rows = map (\i -> V.toList (V.slice (i*n) n v)) [0..n-1]

index :: Int -> (Int, Int) -> Int
index n (row, col) = index0 n (row - 1, col - 1)

index0 :: Int -> (Int, Int) -> Int
index0 n (row, col) = row * n + col