module Main (main) where

import qualified Data.Vector.Unboxed as V
import Matrix
import LU (lu, LU_Output(..), determinant, invert)
import Multiply
import Generate

testEquality :: (Fractional a, Ord a, V.Unbox a) => Matrix a -> Matrix a -> Bool
testEquality a b = size a == size b &&
    V.eqBy (\a b -> (a - b) < 0.001) (values a) (values b)

readMatrixFile :: String -> IO (Maybe (Matrix Double))
readMatrixFile path = do
    contents <- readFile path
    let rows = lines contents
    let values = map read (concatMap words rows)
    let nSquared = length values
    let n = floor (sqrt (fromIntegral nSquared) :: Double)
    if n*n == nSquared then
        return $ Just $ Matrix { size = n, values = V.fromList values }
    else
        return Nothing

main :: IO ()
main = do
    --let path = "examples/inv_eig_matrix(800 x 800).txt"
    let path = "examples/det_matrix(800 x 800).txt"
    Just matrix <- readMatrixFile path
    putStrLn ("matrix loaded: " ++ path)
    let LU_Output l u rank row_swaps = lu matrix
    case multiply l u of
        Just matrix' ->
            putStrLn ("L U = matrix: " ++ show (testEquality matrix' matrix))
        _ ->
            putStrLn "L U"
    putStrLn ("rank: " ++ show rank)
    putStrLn ("determinant: " ++ show (determinant u row_swaps))
    case invert l u of
        Just inv ->
            case multiply matrix inv of
                Just identity' -> do
                    let result = testEquality identity' (identity (size matrix))
                    putStrLn ("inv matrix = id: " ++ show result)
                _ ->
                    putStrLn "matrix inv"
        _ ->
            putStrLn "Couldn't invert"
