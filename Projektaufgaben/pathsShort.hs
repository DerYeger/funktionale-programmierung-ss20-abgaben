module PathsShort where

import Data.List (foldl')

type Array = [[Int]]

get :: Array -> Int -> Int -> Int
get arr y x = (arr !! y) !! x

-- buildArray 3 3 1 2 3
buildArray :: Int -> Int -> Int -> Int -> Int -> Array
buildArray n m z1 z2 z3 = let (_, _, rs) = foldl nextRow (z1, z2, []) [0..n-1] in rs
    where nextRow (z1, z2, rs) i = 
            let (nz1, nz2, r) = buildRow m i (z1, z2, z3) 
            in (nz1, nz2, rs ++ [r]) 

-- buildRow 3 0 (1,2,3)
buildRow :: Int -> Int -> (Int, Int, Int) -> (Int, Int, [Int])
buildRow m i (z1, z2, z3) = foldl' next (z1, z2, []) [0..m-1]
    where next (cz1, cz2, xs) j = 
            let 
                x = ((cz1 * i + cz2 * j) `mod` 9) + 1
                nz1 = (cz1 + z3) `mod` 100
                nz2 = (cz2 * z3) `mod` 100
            in (nz1, nz2, xs ++ [x])
