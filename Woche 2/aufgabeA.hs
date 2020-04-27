isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9' ]

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast list = Just lastElement
    where lastElement = list !! (length list - 1)

reversePair :: (a, b) -> (b, a)
reversePair (a, b) = (b, a)

pair2List :: (a, a) -> [a]
pair2List (first, second) = [first, second]

makeTripel :: a -> b -> c -> (a, b, c)
makeTripel a b c = (a, b, c)

makeTripelV0 :: Num c => a -> b -> (c, a, b)
makeTripelV0 a b = makeTripel 0 a b

makeTripelM1 :: Num c => a -> b -> (a, c, b)
makeTripelM1 a b = makeTripel a 1 b

withFst :: (a -> c) -> (a, b) -> (c, b)
withFst function (first, second) = (function first, second)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list
