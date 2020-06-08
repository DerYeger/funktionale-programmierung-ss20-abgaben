module Groups where

import Data.Function (on)
import Data.List (delete, foldl', sortBy)
import Data.Maybe (isJust, fromJust)

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

-- TODO
getNeighbours :: Solution -> [Solution]
getNeighbours s@(Solution [xs, ys, zs] _) = map asSolution $ getMoveNeighbours s ++ getSwapNeighbours s

getMoveNeighbours :: Solution -> [[Group]]
getMoveNeighbours s
    | length xs == length zs = pure gs -- 3 equal groups
    | length ys > length xs = addCombs xs ys zs ++ addCombs xs zs ys -- 2 large and 1 small group
    | otherwise = addCombs xs zs ys ++ addCombs ys zs xs -- 1 large and 2 small groups
    where 
        gs@[xs, ys, zs] = sortBy (compare `on` length) $ groups s
        addCombs t s n = foldl' (\acc (p, ps) -> [ps, n, p:t]:acc) [] (removeCombs s)
            where removeCombs gs = foldl' (\acc p -> (p, delete p gs):acc) [] gs

getSwapNeighbours :: Solution -> [[Group]]
getSwapNeighbours (Solution gs@[xs, ys, zs] _) = swapAny xs ys zs ++ swapAny xs zs ys ++ swapAny ys zs xs -- get all possible swaps, then sort by length to restore order
    where
        swapAny first second neutral = foldl' (\acc p -> recombine p (delete p first) ++ acc) [] first -- get all possible swaps between the first and second group
            where
                recombine p ps = foldl' (\acc (u, us) -> [p:us, u:ps, neutral]:acc) [] secondRemoved -- combine this removal from the first group with every possible removal from the second group
                secondRemoved = foldl' (\acc p -> (p, delete p second):acc) [] second -- get all possible removals from the second group

localSearch :: Solution -> IO Solution
localSearch s = do
    print s
    let ns = getNeighbours s
    let best = foldl' (\b n -> if score n > score b then n else b) (head ns) (tail ns) -- This won't actually cause problems. Neighbours should never be empty.
    if score best <= score s then return s else localSearch best

main :: IO ()
main = do 
    ls <- lines <$> readFile "wishes.txt"
    let gs = partitionGroups $ map (asPerson . words) ls
    localSearch $ asSolution gs
    return ()
