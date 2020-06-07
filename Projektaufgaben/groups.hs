import Data.List.Split (chunksOf)
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)

data Person = Person{name::String, wishes::[String]} deriving (Eq)
instance Show Person where
    show (Person n _) = show n

type Group = [Person]

wishScores :: [Int]
wishScores = [10, 5, 1]

asPerson :: [String] -> Person
asPerson (n:xs) = Person n $ take 3 xs

optimizeGroups :: String -> IO ()
optimizeGroups fileName = do
    fileLines <- lines <$> readFile fileName
    let persons = map (asPerson . words) fileLines
    let groups = asGroups persons
    print persons
    print groups
    print $ totalScore groups

asGroups :: Group -> [Group]
asGroups = foldr partition [[], [], []]
    where partition x [xs, ys, zs] = [ys, zs, x:xs]

totalScore :: [Group] -> Int
totalScore = foldl' (\acc g -> acc + groupScore g) 0
    where groupScore g = foldl' personScore 0 g
            where 
                personScore acc (Person _ ws) = foldl' (+) acc $ zipWith checkWish ws wishScores
                checkWish w s = if foldr (\x acc -> acc || (name x == w)) False g then s else 0

main :: IO ()
main = optimizeGroups "wishes.txt"