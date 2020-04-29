-- Infos

-- Juri Lozowoj, 35244015, Bachelor PO2010
-- Jan Müller, 35011918, Bachelor PO2010

-- Aufgabe a

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

makeTripelV0 :: a -> b -> (Integer, a, b)
makeTripelV0 = makeTripel 0

makeTripelM1 :: a -> b -> (a, Integer, b)
makeTripelM1 a b = makeTripel a 1 b

withFst :: (a -> c) -> (a, b) -> (c, b)
withFst function (first, second) = (function first, second)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

-- Aufgabe b

safetail :: [a] -> [a]
safetail [] = []
safetail (head:list) = list

one2two :: [a] -> ([a], [a])
one2two list = (everySecond list, everySecond . safetail $ list)
    where 
        everySecond [] = []
        everySecond (first:rest) = first:(everySecond . safetail $ rest)              

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys 
merge (x:xs) (y:ys) 
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

mergesort :: Ord a => [a] -> [a]
mergesort list =
    case list of
        [] -> []
        [a] -> [a]
        otherwise -> merge (mergesort $ take half list) (mergesort $ drop half list)
            where half = (length list) `div` 2

remove_ao :: String -> String
remove_ao input = map (\char -> if (char `elem` ['a', 'o']) then '_' else char) input

reversePairList :: [(a, b)] -> [(b, a)]
reversePairList list = reverse [(b, a) | (a, b) <- list]

remove_01 :: [(Int, Int)] -> [(Int, Int)]
remove_01 list = filter (\(first, second) -> first /= 0 && second /= 1) list

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates list = 
    let
        helper [] acc = acc
        helper (head:tail) acc = if (head `elem` acc)
            then helper tail acc -- element is a duplicate so we ignore it
            else helper tail (head:acc) -- element is new so we add it to the accumulator
    in reverse $ helper list []
    
-- Aufgabe c

-- Es wurden alle Sprachkosntrukte aus b) (if-else, Wächter, Mustererkennung ohne case, Mustererkennung mit case, anonyme Funktion, where, let, Eigenschaftsliste, filter, Verkettung von Funktionen (Operator “.”), map, $) verwendet. Daher werden hier keine weiteren Funktionen implementiert. 
