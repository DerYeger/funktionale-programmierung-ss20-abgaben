module PathsShort where

import Data.List (foldl')

type Array = [[Int]]

data Step = Step {y::Int, x::Int, val::Int, score::Int}
instance Show Step where
    show (Step y x val score) = "(" ++ show y ++ "," ++ show x ++ "," ++ show val ++ "," ++ show score ++ ")"
instance Eq Step where
    (Step fy fx _ _) == (Step sy sx _ _) = fy == sy && fx == sx

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

initialState arr y x = Step y x val val
    where val = get arr y x

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

-- nextSteps 3 3 (0, 1) testArray [initialState testArray 1 1]
nextSteps :: Int -> Int -> (Int, Int) -> Array -> [Step] -> [[Step]]
nextSteps n m (yt, xt) arr steps@(s@(Step y x _ _):_) = addStepU . addStepL . addStepD . addStepR $ []
    where
        addStepR = addIfValid (stepR arr s)
        addStepD = addIfValid (stepD arr s)
        addStepL = addIfValid (stepL arr s)
        addStepU = addIfValid (stepU arr s)
        addIfValid ns xs = if isValid ns then (ns : steps) : xs else xs
        -- isValid <-> score is positive and step is not the target without having visited others and step is no repeat
        isValid ns = isInRange ns && score ns >= 0 && noPrematureEnd ns && ns `notElem` steps
        noPrematureEnd (Step y x _ _) = not (y == yt && x == xt && length steps <= n * m - 1)
        isInRange (Step y x _ _) = 0 <= y && y < n && 0 <= x && x < m

testArray = buildArray 3 3 1 2 3
