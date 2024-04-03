module Multiply where

import qualified Data.Vector.Unboxed as V

import Matrix (Matrix(..), index0)

multiply :: (Num a, V.Unbox a) => Matrix a -> Matrix a -> Maybe (Matrix a)
multiply a b =
    if size a /= size b then
        Nothing
    else
        Just (Matrix { size = n, values = V.fromList (map value positions) })
    where
    n = size a
    axis = [0..n-1]
    positions = [(row, col) | row <- axis, col <- axis]
    value (row, col) = sum (map go axis) where
        go i = V.unsafeIndex (values a) (index0 n (row, i))
            * V.unsafeIndex (values b) (index0 n (i, col))