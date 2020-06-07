import Data.List.Split (chunksOf)
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)

data Person = Person{name::String, wishes::[String]} deriving (Eq)
instance Show Person where
    show (Person n _) = show n

type Group = [Person]

personify :: [String] -> Person
personify (n:xs) = Person n $ take 3 xs

myRead :: String -> IO ()
myRead name = do
    putStrLn name
    inhalt <- readFile name
    let persons = map (personify . words) (lines inhalt)
    let groups = asGroups persons
    print persons
    print groups
    print $ totalScore groups

asGroups :: Group -> [Group]
asGroups = foldr partition [[], [], []]
    where partition x [xs, ys, zs] = [ys, zs, x:xs]

totalScore :: [Group] -> Int
totalScore = foldl' (\acc g -> acc + groupScore g) 0
    where groupScore group = foldl' calc 0 group
            where 
                calc :: Int -> Person -> Int
                calc acc (Person _ ws) = foldl' (+) acc $ zipWith checkWish ws [10, 5, 1]
                    where 
                        checkWish :: String -> Int -> Int
                        checkWish wish score = if foldr (\x acc -> acc || (name x == wish)) False group then score else 0

main :: IO ()
main = myRead "wish.txt"