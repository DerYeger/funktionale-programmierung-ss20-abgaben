module Groups where

import Data.List.Split (chunksOf)
import Data.List (delete, foldl')
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
-- important ! keep groups ordered by size
getNeighbours :: Solution -> [Solution]
getNeighbours s@(Solution [xs, ys, zs] _) = map asSolution (getMoveNeighbours s)

getMoveNeighbours :: Solution -> [[Group]]
getMoveNeighbours s@(Solution gs@[small, middle, large] _)
    | length (head gs) == length (gs !! 2) = pure gs --swapOne small middle large ++ swapOne small large middle ++ swapOne middle large small
    | length (gs !! 1) > length (head gs) = addCombs small middle large ++ addCombs small large middle -- 2 large and 1 small group
    | otherwise = addCombs small large middle ++ addCombs middle large small -- 1 large and 2 small groups
    where 
        addCombs target source neutral = foldl' (\acc (p, ps) -> [ps, neutral, p:target]:acc) [] (removeCombs source)
        --swapOne source_one source_two neutral = foldl' (\acc (removed_one, rest1, removed_two, rest2) -> [removed_two:rest1, removed_one:rest2, neutral]:acc) [] (removeTwo source_one source_two)
        toSwap = (removeTwo g_one g_two) 
        swapOne source_one source_two neutral = addSecond $ ((addFirst toSwap neutral) toSwap)
            where 
                removeCombs gs = foldl' (\acc p -> (p, delete p gs):acc) [] gs
                removeTwo g_one g_two = foldl' (\acc p -> ((p, delete p g_one), removeCombs g_two): acc) [] g_one
                addFirst gs n = foldl' (\acc (f, f_rest, _) -> [f_rest, n, f]:acc) [] gs
                addSecond gs swapInfo =

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
