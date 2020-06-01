module PathsShort where

import Data.List (foldl')

-- buildRows 3 3 1 2 3
buildRows :: Int -> Int -> Int -> Int -> Int -> [[Int]]
buildRows n m z1 z2 z3 = let (_, _, _, rs) = foldl nextRow (z1, z2, z3, []) [0..n-1] in rs
    where nextRow (z1, z2, z3, rs) i = 
            let (nz1, nz2, nz3, r) = buildRow m i (z1, z2, z3) 
            in (nz1, nz2, nz3, rs ++ [r]) 

-- buildRow 3 0 (1,2,3)
buildRow :: Int -> Int -> (Int, Int, Int) -> (Int, Int, Int, [Int])
buildRow m i (z1, z2, z3) = foldl' next (z1, z2, z3, []) [0..m-1]
    where next (z1, z2, z3, xs) j = 
            let 
                x = ((z1 * i + z2 * j) `mod` 9) + 1
                nz1 = (z1 + z3) `mod` 100
                nz2 = (z2 * z3) `mod` 100
            in (nz1, nz2, z3, xs ++ [x])
