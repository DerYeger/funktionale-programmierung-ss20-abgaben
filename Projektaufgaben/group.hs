import Data.List.Split (chunksOf)

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

asGroups :: Group -> [Group]
asGroups = foldr partition [[], [], []]
    where partition x [xs, ys, zs] = [ys, zs, x:xs]

main :: IO ()
main = myRead "wish.txt"