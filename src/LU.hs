module LU where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad (forM, forM_, when)
import Data.List (find)
import Data.STRef

import Matrix (Matrix(..), index0)
import Generate (identity, one)
import Solve

data LU_Output a = LU_Output
    { l :: Matrix a
    , u :: Matrix a
    , rank :: Int
    , row_swaps :: Int
    }
    deriving Show

lu :: (Fractional a, Ord a, V.Unbox a) => Matrix a -> LU_Output a
lu matrix = runST $ do
    let n = size matrix
    let at row col = row * n + col
    mut_l <- V.thaw (values (identity n))
    mut_u <- V.thaw (values matrix)
    mut_rank <- newSTRef n
    mut_swaps <- newSTRef 0
    forM_ [0..n-1] $ \i -> do
        pairs <- forM [i..n-1] $ \row -> do
            value <- MV.unsafeRead mut_u (at row i)
            return (value, row)
        case find ((/= 0) . fst) pairs of
            Just (i_i, pivot_row) -> do
                when (i /= pivot_row) $ do
                    modifySTRef mut_swaps succ
                    forM_ [0..n-1] $ \col -> do
                        MV.unsafeSwap mut_l (at pivot_row col) (at i col)
                        MV.unsafeSwap mut_u (at pivot_row col) (at i col)
                forM_ [i+1..n-1] $ \row -> do
                    row_i <- MV.unsafeRead mut_u (at row i)
                    let ratio = row_i / i_i
                    forM_ [i..n-1] $ \col -> do
                        i_col <- MV.unsafeRead mut_u (at i col)
                        MV.modify mut_u (subtract (i_col * ratio)) (at row col)
                    MV.write mut_l (at row i) ratio
            _ ->
                modifySTRef mut_rank (subtract 1)
    l_values <- V.freeze mut_l
    u_values <- V.freeze mut_u
    rank_value <- readSTRef mut_rank
    swaps_value <- readSTRef mut_swaps
    return $ LU_Output
        { l = Matrix { size = n, values = l_values }
        , u = Matrix { size = n, values = u_values }
        , rank = rank_value
        , row_swaps = swaps_value
        }

diagonal_product :: (Num a, V.Unbox a) => Matrix a -> a
diagonal_product matrix = product (map unsafeRead diagonal) where
    n = size matrix
    unsafeRead = V.unsafeIndex (values matrix)
    diagonal = map (\i -> index0 n (i, i)) [0..n-1]

determinant :: (Num a, V.Unbox a) => Matrix a -> Int -> a
determinant u row_swaps = sign (diagonal_product u) where
    sign = if row_swaps `mod` 2 == 0 then id else negate

invert :: (Fractional a, V.Unbox a) => Matrix a -> Matrix a -> Maybe (Matrix a)
invert l u = do
    let n = size l
    return $ runST $ do
        mut_values <- MV.unsafeNew (n * n)
        forM_ [0..n-1] $ \col ->
            case solve l u (one n col) of
                Just column ->
                    forM [0..n-1] $ \row -> do
                        let value = V.unsafeIndex column row
                        MV.unsafeWrite mut_values (index0 n (row, col)) value
                _ ->
                    return []
        xs <- V.freeze mut_values
        return $ Matrix { size = n, values = xs }