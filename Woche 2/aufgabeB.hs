safeTail :: [a] -> [a]
safeTail [] = []
safeTail (head:list) = list

one2two :: [a] -> ([a], [a])
one2two list = (everySecond list, everySecond . safeTail $ list)
    where 
        everySecond [] = []
        everySecond (first:rest) = first:(everySecond . safeTail $ rest)              

merge :: Ord a => [a] -> [a] -> [a]
merge [] secondList = secondList
merge firstList [] = firstList
merge firstList secondList
    | firstHead <= secondHead = firstHead:(merge firstRest secondList)
    | otherwise = secondHead:(merge firstList secondRest)
        where 
            (firstHead:firstRest) = firstList
            (secondHead:secondRest) = secondList

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
    in
        reverse $ helper list []
