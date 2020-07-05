-- Compilation: ghc -main-is Groups groups.hs
-- Execution: groups wishes.txt
-- GHCi: stack ghci groups.hs
    -- Usage: evaluateGroups "wishes.txt"

module Groups where

import Data.List (delete, foldl', maximumBy, sortOn)
import Data.Ord (comparing)
import System.Environment (getArgs)

data Person = Person {name::String, wishes::[String]} 
    deriving (Eq)
instance Show Person where
    show (Person n _) = show n

type Group = [Person]

data Solution = Solution {groups::[Group], score::Int}
    deriving (Show)

wishScores :: [Int]
wishScores = [10, 5, 1]

asPerson :: [String] -> Person
asPerson (n:xs) = Person n $ take 3 xs

partitionGroups :: Group -> [Group]
partitionGroups = foldr partition [[], [], []]
    where partition x [xs, ys, zs] = [ys, zs, x:xs]

totalScore :: [Group] -> Int
totalScore = foldl' (\acc g -> acc + groupScore g) 0
    where groupScore g = foldl' personScore 0 g
            where 
                personScore acc (Person _ ws) = foldl' (+) acc $ zipWith checkWish ws wishScores
                checkWish w s = if foldr (\x acc -> acc || (name x == w)) False g then s else 0

asSolution :: [Group] -> Solution
asSolution gs = Solution gs $ totalScore gs

removeSinglePerson :: Group -> [(Person, Group)]
removeSinglePerson gs = foldl' (\acc p -> (p, delete p gs):acc) [] gs

getNeighbours :: Solution -> [Solution]
getNeighbours s = map asSolution $ getMoveNeighbours s ++ getSwapNeighbours s

getMoveNeighbours :: Solution -> [[Group]]
getMoveNeighbours s
    | length xs == length zs = [] -- 3 equal groups
    | length ys > length xs = addCombs xs ys zs ++ addCombs xs zs ys -- 2 large and 1 small group
    | otherwise = addCombs xs zs ys ++ addCombs ys zs xs -- 1 large and 2 small groups
    where 
        gs@[xs, ys, zs] = sortOn length $ groups s
        addCombs t s n = foldl' (\acc (p, ps) -> [ps, n, p:t]:acc) [] $ removeSinglePerson s

getSwapNeighbours :: Solution -> [[Group]]
getSwapNeighbours (Solution gs@[xs, ys, zs] _) = fs ++ ft ++ st ++ swapAgain fs ++ swapAgain ft ++ swapAgain st
    where
        fs = swapAny xs ys zs
        ft = swapAny xs zs ys
        st = swapAny ys zs xs
        swapAgain = concatMap (\(f:(s:(n:_))) -> swapAny f s n)
        swapAny first second neutral = foldl' (\acc p -> recombine p (delete p first) ++ acc) [] first -- get all possible swaps between the first and second group
            where
                recombine p ps = foldl' (\acc (u, us) -> [p:us, u:ps, neutral]:acc) [] secondRemoved -- combine this removal from the first group with every possible removal from the second group
                secondRemoved = removeSinglePerson second -- get all possible removals from the second group

localSearch :: Solution -> IO Solution
localSearch s = do
    print s
    let ns = getNeighbours s
    let best = maximumBy (comparing score) ns
    if score best <= score s then return s else localSearch best

evaluateGroups :: String -> IO ()
evaluateGroups fileName = do
    fileContents <- readFile fileName
    let gs = partitionGroups $ map (asPerson . words) $ lines fileContents
    localSearch $ asSolution gs
    return ()

main :: IO ()
main = getArgs >>= evaluateGroups . head
