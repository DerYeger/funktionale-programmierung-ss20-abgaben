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
    
mergesort_let :: (Ord a) => [a] -> [a]
mergesort_let xs 
        | (length xs) <= 1 = xs
        | otherwise = 
            let
                ls = take ((length xs) `div` 2) xs
                rs = drop ((length xs) `div` 2) xs
            in merge (mergesort_let ls) (mergesort_let rs)
