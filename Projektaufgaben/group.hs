import Data.List.Split (chunksOf)
import Data.List (foldl')

data Person = Person{name::String, first::Maybe String, second::Maybe String, third::Maybe String} deriving (Eq)
instance Show Person where
    show (Person n f s t) = show n

type Group = [Person]

personify :: [String] -> Person
personify (n:f:s:t:_) = Person n (Just f) (Just s) (Just t)
personify (n:f:s:_) = Person n (Just f) (Just s) Nothing
personify (n:f:_) = Person n (Just f) Nothing Nothing
personify (n:_) = Person n Nothing Nothing Nothing

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
                containsWish n = foldr (\x acc -> acc || (name x == n)) False group
                calc :: Int -> Person -> Int
                calc acc (Person _ f s t) = acc + checkWish f 10 + checkWish s 5 + checkWish t 1
                    where 
                        checkWish wish score = case wish of
                            Nothing -> 0
                            Just n -> if containsWish n then score else 0

main :: IO ()
main = myRead "wish.txt"