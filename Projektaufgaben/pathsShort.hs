-- GHCi: ghci pathsShort.hs
    -- Usage: pathsShort n m is js ie je z1 z2 z3
        -- pathsShort 3 3 0 0 2 2 1 2 3
        -- pathsShort 2 3 0 0 1 2 1 2 3
        -- pathsShort 2 4 0 0 1 3 123 456 789
        -- pathsShort 2 4 0 0 1 2 5 5 5

module PathsShort (pathsShort) where

import Data.List (foldl')

data Step = Step {y :: Int, x :: Int, val :: Int, score :: Int}

instance Show Step where
  show (Step y x val score) = "(" ++ show y ++ "," ++ show x ++ "," ++ show val ++ "," ++ show score ++ ")"

instance Eq Step where
  (Step fy fx _ _) == (Step sy sx _ _) = fy == sy && fx == sx

buildArray :: Int -> Int -> Int -> Int -> Int -> [[Int]]
buildArray n m z1 z2 z3 = let (_, _, rs) = foldl' nextRow (z1, z2, []) [0 .. n -1] in rs
  where
    nextRow (z1, z2, rs) i =
      let (nz1, nz2, r) = buildRow m i z1 z2 z3
       in (nz1, nz2, rs ++ [r])

buildRow :: Int -> Int -> Int -> Int -> Int -> (Int, Int, [Int])
buildRow m i z1 z2 z3 = foldl' next (z1, z2, []) [0 .. m -1]
  where
    next (cz1, cz2, xs) j =
      let x = ((cz1 * i + cz2 * j) `mod` 9) + 1
       in ((cz1 + z3) `mod` 100, (cz2 * z3) `mod` 100, xs ++ [x])

pathsShort :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Step]]
pathsShort n m ys xs yt xt z1 z2 z3 = map reverse $ iterate (concatMap nextSteps) [[Step ys xs sVal sVal]] !! (n * m - 1)
  where
    sVal = (arr !! ys) !! xs
    arr = buildArray n m z1 z2 z3
    nextSteps steps@(Step y x _ cScore : _) = foldl' validStep [] [(-1, 0, div), (0, -1, (-)), (1, 0, (*)), (0, 1, (+))]
      where
        validStep acc (yd, xd, op) = if isInRange && noPrematureEnd && score step >= 0 && step `notElem` steps then (step : steps) : acc else acc
          where
            ny = y + yd
            nx = x + xd
            isInRange = 0 <= ny && ny < n && 0 <= nx && nx < m
            noPrematureEnd = ny /= yt || nx /= xt || length steps == n * m - 1
            step = Step ny nx nVal (cScore `op` nVal)
              where
                nVal = (arr !! ny) !! nx
