module PathsShort where

import Data.List (foldl')

data Step = Step {y::Int, x::Int, val::Int, score::Int}
instance Show Step where
    show (Step y x val score) = "(" ++ show y ++ "," ++ show x ++ "," ++ show val ++ "," ++ show score ++ ")"
instance Eq Step where
    (Step fy fx _ _) == (Step sy sx _ _) = fy == sy && fx == sx

-- buildArray 3 3 1 2 3
buildArray :: Int -> Int -> Int -> Int -> Int -> [[Int]]
buildArray n m z1 z2 z3 = let (_, _, rs) = foldl' nextRow (z1, z2, []) [0..n-1] in rs
    where nextRow (z1, z2, rs) i = 
            let (nz1, nz2, r) = buildRow m i z1 z2 z3
            in (nz1, nz2, rs ++ [r]) 

-- buildRow 3 0 (1,2,3)
buildRow :: Int -> Int -> Int -> Int -> Int -> (Int, Int, [Int])
buildRow m i z1 z2 z3 = foldl' next (z1, z2, []) [0..m-1]
    where next (cz1, cz2, xs) j = 
            let x = ((cz1 * i + cz2 * j) `mod` 9) + 1
            in ((cz1 + z3) `mod` 100, (cz2 * z3) `mod` 100, xs ++ [x])

-- pathsShort 3 3 0 0 2 2 1 2 3
-- pathsShort 2 3 0 0 1 2 1 2 3
-- pathsShort 2 4 0 0 1 3 123 456 789
-- pathsShort 2 4 0 0 1 2 5 5 5
pathsShort :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Step]]
pathsShort n m ys xs yt xt z1 z2 z3 = map reverse paths
    where 
        sVal = (arr !! ys) !! xs
        arr = buildArray n m z1 z2 z3
        paths = iterate (concatMap nextSteps) [[Step ys xs sVal sVal]] !! (n * m - 1)
        nextSteps steps@(s@(Step y x _ score):_) = addIfValid (move (y-1) x div) . addIfValid (move y (x-1) (-)) . addIfValid (move (y+1) x (*)) . addIfValid (move y (x+1) (+)) $ []
            where
                move ny nx op = Step ny nx nVal (score `op` nVal)
                    where nVal = (arr !! ny) !! nx
                addIfValid ns@(Step y x _ nscore) ps = if isInRange && nscore >= 0 && noPrematureEnd && ns `notElem` steps then (ns : steps) : ps else ps
                    where 
                        isInRange = 0 <= y && y < n && 0 <= x && x < m
                        noPrematureEnd = y /= yt || x /= xt || length steps == n * m - 1
