-- Compilation: ghc -main-is Groups groups.hs
    -- Execution: groups wishes.txt
-- GHCi: ghci groups.hs
    -- Usage: :main wishes.txt

module Groups (main) where

import Control.Monad (void)
import Data.List (delete, foldl', maximumBy, sortOn)
import Data.Ord (comparing)
import System.Environment (getArgs)

data Person = Person {name :: String, wishes :: [String]}
  deriving (Eq)

instance Show Person where
  show (Person n _) = show n

type Group = [Person]

data Solution = Solution {groups :: [Group], score :: Int}
  deriving (Show)

sumBy :: (Foldable f) => (a -> Int) -> f a -> Int
sumBy f = foldl' (flip $ (+) . f) 0

totalScore :: [Group] -> Int
totalScore = sumBy groupScore
  where
    groupScore g = sumBy personScore g
      where
        personScore (Person _ ws) = sum $ zipWith checkWish ws [10, 5, 1]
        checkWish w s = if foldr (\x acc -> name x == w || acc) False g then s else 0

asSolution :: [Group] -> Solution
asSolution gs = Solution gs $ totalScore gs

removeSinglePerson :: Group -> [(Person, Group)]
removeSinglePerson gs = foldl' (\acc p -> (p, delete p gs) : acc) [] gs

getNeighbours :: Solution -> [Solution]
getNeighbours s = map asSolution $ getMoveNeighbours s ++ getSwapNeighbours s

getMoveNeighbours :: Solution -> [[Group]]
getMoveNeighbours s
  | length xs == length zs = [] -- 3 equal groups
  | length ys > length xs = movePerson xs ys zs ++ movePerson xs zs ys -- 2 large and 1 small group
  | otherwise = movePerson xs zs ys ++ movePerson ys zs xs -- 1 large and 2 small groups
  where
    [xs, ys, zs] = sortOn length $ groups s
    movePerson t s n = foldl' (\acc (p, ps) -> [ps, n, p : t] : acc) [] $ removeSinglePerson s

getSwapNeighbours :: Solution -> [[Group]]
getSwapNeighbours (Solution [xs, ys, zs] _) = allSwaps xs ys zs ++ allSwaps xs zs ys ++ allSwaps ys zs xs
  where
    allSwaps first second neutral = singleSwapped ++ concatMap (\[f, s, n] -> swapPerson f s n) singleSwapped
      where
        singleSwapped = swapPerson first second neutral
    swapPerson first second neutral = foldl' (\acc p -> recombine p (delete p first) ++ acc) [] first -- get all possible swaps between the first and second group
      where
        recombine p ps = foldl' (\acc (u, us) -> [p : us, u : ps, neutral] : acc) [] secondRemoved -- combine this removal from the first group with every possible removal from the second group
        secondRemoved = removeSinglePerson second -- get all possible removals from the second group

localSearch :: Solution -> IO Solution
localSearch s =
  let best = maximumBy (comparing score) $ getNeighbours s
   in print s >> if score best <= score s then return s else localSearch best

main :: IO ()
main = void . localSearch =<< parseFile . head =<< getArgs
  where
    parseFile path = asSolution . partitionGroups . map (asPerson . words) . lines <$> readFile path
    asPerson (n : xs) = Person n $ take 3 xs
    partitionGroups = foldl' partition [[], [], []]
      where
        partition [xs, ys, zs] x = [ys, zs, x : xs]
