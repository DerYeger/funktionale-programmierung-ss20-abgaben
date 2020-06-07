import Data.List.Split

data Person = Person{name::String, first::String, second::String, third::String} deriving (Eq)
instance Show Person where
    show (Person n f s t) = show n

personify :: [String] -> Person
personify (n:xs) = (Person n _ _ _)
personify (n:f:xs) = (Person n f _ _)
personify (n:f:s:xs) = (Person n f s _)
personify (n:f:s:t:xs) = (Person n f s t)


myRead :: String -> IO Int
myRead name = do
    putStrLn name
    inhalt <- readFile name
    let lineCount = length $ lines inhalt
    let groupSize = (lineCount `div` 3) + 1
    let myLines = lines inhalt
    let participants = map words myLines
    let firstGroup_x = map (splitOn " ") $ take groupSize myLines
    --let firstGroup = map head firstGroup_x
    let firstGroup = map personify firstGroup_x
    let secondGroup_x = map (splitOn " ") $ take groupSize (drop groupSize myLines)
    let secondGroup = map head secondGroup_x 
    let thirdGroup_x = map (splitOn " ") $ take groupSize (drop (groupSize * 2) myLines) 
    let thirdGroup = map head thirdGroup_x
    print firstGroup
    print secondGroup 
    print thirdGroup
    print firstGroup_x
    print secondGroup_x 
    print thirdGroup_x
    return lineCount 

main :: IO Int
main = myRead "wish.txt"