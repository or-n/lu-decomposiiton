module Solve where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad (forM_, guard)

import Matrix

solve :: (Fractional a, V.Unbox a) => Matrix a -> Matrix a -> V.Vector a
      -> Maybe (V.Vector a)
solve l u out = do
    guard (size l == V.length out)
    mid <- solve_l l out
    solve_u u mid

solve_l :: (Num a, V.Unbox a) => Matrix a -> V.Vector a -> Maybe (V.Vector a)
solve_l matrix out = do
    let n = size matrix
    guard (n == V.length out)
    let v = values matrix
    return $ runST $ do
            mut_weights <- V.thaw out
            forM_ [0..n-1] $ \row ->
                forM_ [0..row-1] $ \col -> do
                    let scale = V.unsafeIndex v (index0 n (row, col))
                    value <- MV.unsafeRead mut_weights col
                    MV.modify mut_weights (subtract (value * scale)) row
            V.freeze mut_weights

solve_u :: (Fractional a, V.Unbox a) => Matrix a -> V.Vector a
        -> Maybe (V.Vector a)
solve_u matrix out = do
    let n = size matrix
    guard (n == V.length out)
    let v = values matrix
    return $ runST $ do
            mut_weights <- V.thaw out
            forM_ (reverse [0..n-1]) $ \row -> do
                forM_ [row+1..n-1] $ \col -> do
                    let scale = V.unsafeIndex v (index0 n (row, col))
                    value <- MV.unsafeRead mut_weights col
                    MV.modify mut_weights (subtract (value * scale)) row
                let scale = V.unsafeIndex v (index0 n (row, row))
                MV.modify mut_weights (/ scale) row
            V.freeze mut_weights