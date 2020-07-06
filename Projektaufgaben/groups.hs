-- Compilation: ghc -main-is Groups groups.hs
-- Execution: groups wishes.txt

module Groups where

import Control.Monad (void)
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
partitionGroups = foldl' partition [[], [], []]
    where partition [xs, ys, zs] x = [ys, zs, x:xs]

totalScore :: [Group] -> Int
totalScore = foldl' (\acc g -> acc + groupScore g) 0
    where groupScore g = foldl' personScore 0 g
            where 
                personScore acc (Person _ ws) = foldl' (+) acc $ zipWith checkWish ws wishScores
                checkWish w s = if foldr (\x acc -> name x == w || acc) False g then s else 0

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
        [xs, ys, zs] = sortOn length $ groups s
        addCombs t s n = foldl' (\acc (p, ps) -> [ps, n, p:t]:acc) [] $ removeSinglePerson s

getSwapNeighbours :: Solution -> [[Group]]
getSwapNeighbours (Solution [xs, ys, zs] _) = allSwaps xs ys zs ++ allSwaps xs zs ys ++ allSwaps ys zs xs
    where
        allSwaps first second neutral = singleSwapped ++ concatMap (\(f:(s:(n:_))) -> swapSingle f s n) singleSwapped
            where singleSwapped = swapSingle first second neutral
        swapSingle first second neutral = foldl' (\acc p -> recombine p (delete p first) ++ acc) [] first -- get all possible swaps between the first and second group
            where
                recombine p ps = foldl' (\acc (u, us) -> [p:us, u:ps, neutral]:acc) [] secondRemoved -- combine this removal from the first group with every possible removal from the second group
                secondRemoved = removeSinglePerson second -- get all possible removals from the second group

localSearch :: Solution -> IO Solution
localSearch s = do
    print s
    let best = maximumBy (comparing score) $ getNeighbours s
    if score best <= score s then return s else localSearch best

main :: IO ()
main = void . localSearch =<< parseFile . head =<< getArgs
    where parseFile path = asSolution . partitionGroups . map (asPerson . words) . lines <$> readFile path
