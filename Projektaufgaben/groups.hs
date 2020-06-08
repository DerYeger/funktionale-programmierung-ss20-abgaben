module Groups where

import Data.List.Split (chunksOf)
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)

data Person = Person {name::String, wishes::[String]}
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
getNeighbours = pure

lokalSearch :: Solution -> IO Solution
lokalSearch s = do
    print s
    let ns = getNeighbours s
    let best = foldl' (\b n -> if score n > score b then n else b) (head ns) (tail ns) -- This won't actually cause problems. Neighbours should never be empty.
    if score best <= score s then return s else loalSearch best

main :: IO ()
main = do 
    ls <- lines <$> readFile "wishes.txt"
    let gs = partitionGroups $ map (asPerson . words) ls
    loalSearch $ asSolution gs
    return ()
