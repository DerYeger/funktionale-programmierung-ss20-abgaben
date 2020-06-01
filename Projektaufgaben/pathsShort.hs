module PathsShort where

import Data.List (foldl')

type Array = [[Int]]

data State = State {y::Int, x::Int, val::Int, score::Int}
instance Show State where
    show (State y x val score) = "(" ++ show y ++ "," ++ show x ++ "," ++ show val ++ "," ++ show score ++ ")"

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

initialState arr y x = State y x val val
    where val = get arr y x

stepR arr (State y x val score) = State y nx nVal (score + nVal)
    where 
        nx = x + 1
        nVal = get arr y nx

stepD arr (State y x val score) = State ny x nVal (score * nVal)
    where 
        ny = y + 1
        nVal = get arr ny x

stepL arr (State y x val score) = State y nx nVal (score - nVal)
    where 
        nx = x - 1
        nVal = get arr y nx

stepU arr (State y x val score) = State ny x nVal (score `div` nVal)
    where 
        ny = y - 1
        nVal = get arr ny x

-- nextStates 3 3 testArray [initialState testArray 1 1]
nextStates :: Int -> Int -> Array -> [State] -> [[State]]
nextStates m n arr steps@(s:_) = addStepU . addStepL . addStepD . addStepR $ []
    where
        addStepR xs = if x s < m - 1 then (stepR arr s : steps) : xs else xs
        addStepD xs = if x s < n - 1 then (stepD arr s : steps) : xs else xs
        addStepL xs = if x s > 0 then (stepL arr s : steps) : xs else xs
        addStepU xs = if y s > 0 then (stepU arr s : steps) : xs else xs

testArray = buildArray 3 3 1 2 3
