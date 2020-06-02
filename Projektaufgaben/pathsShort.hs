module PathsShort where

import Data.List (foldl')

type Array = [[Int]]

data Step = Step {y::Int, x::Int, val::Int, score::Int}
instance Show Step where
    show (Step y x val score) = "(" ++ show y ++ "," ++ show x ++ "," ++ show val ++ "," ++ show score ++ ")"
instance Eq Step where
    (Step fy fx _ _) == (Step sy sx _ _) = fy == sy && fx == sx

type Path = [Step]

get :: Array -> Int -> Int -> Int
get arr y x = (arr !! y) !! x

-- buildArray 3 3 1 2 3
buildArray :: Int -> Int -> Int -> Int -> Int -> Array
buildArray n m z1 z2 z3 = let (_, _, rs) = foldl nextRow (z1, z2, []) [0..n-1] in rs
    where nextRow (z1, z2, rs) i = 
            let (nz1, nz2, r) = buildRow m i z1 z2 z3
            in (nz1, nz2, rs ++ [r]) 

-- buildRow 3 0 (1,2,3)
buildRow :: Int -> Int -> Int -> Int -> Int -> (Int, Int, [Int])
buildRow m i z1 z2 z3 = foldl' next (z1, z2, []) [0..m-1]
    where next (cz1, cz2, xs) j = 
            let 
                x = ((cz1 * i + cz2 * j) `mod` 9) + 1
                nz1 = (cz1 + z3) `mod` 100
                nz2 = (cz2 * z3) `mod` 100
            in (nz1, nz2, xs ++ [x])

stepR arr (Step y x val score) = Step y nx nVal (score + nVal)
    where 
        nx = x + 1
        nVal = get arr y nx

stepD arr (Step y x val score) = Step ny x nVal (score * nVal)
    where 
        ny = y + 1
        nVal = get arr ny x

stepL arr (Step y x val score) = Step y nx nVal (score - nVal)
    where 
        nx = x - 1
        nVal = get arr y nx

stepU arr (Step y x val score) = Step ny x nVal (score `div` nVal)
    where 
        ny = y - 1
        nVal = get arr ny x

-- pathsShort 3 3 0 0 2 2 1 2 3
-- pathsShort 2 3 0 0 1 2 1 2 3
-- pathsShort 2 4 0 0 1 3 123 456 789
-- pathsShort 2 4 0 0 1 2 5 5 5
pathsShort n m ys xs yt xt z1 z2 z3 = map reverse paths
    where 
        sVal = get arr ys xs
        arr = buildArray n m z1 z2 z3
        paths = iterate (concatMap nextSteps) [[Step ys xs sVal sVal]] !! (n * m - 1)
        nextSteps steps@(s@(Step y x _ _):_) = addIfValid (stepU arr s) . addIfValid (stepL arr s) . addIfValid (stepD arr s) . addIfValid (stepR arr s) $ []
            where
                addIfValid n ps = if isValid n then (n : steps) : ps else ps
                isValid n = isInRange n && score n >= 0 && noPrematureEnd n && n `notElem` steps
                isInRange (Step y x _ _) = 0 <= y && y < n && 0 <= x && x < m
                noPrematureEnd (Step y x _ _) = y /= yt || x /= xt || length steps == n * m - 1
