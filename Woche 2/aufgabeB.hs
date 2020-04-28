safetail :: [a] -> [a]
safetail [] = []
safetail (head:list) = list

one2two :: [a] -> ([a], [a])
one2two list = (everySecond list, everySecond . safeTail $ list)
    where 
        everySecond [] = []
        everySecond (first:rest) = first:(everySecond . safeTail $ rest)              

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys 
merge (x:xs) (y:ys) 
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
        | (length xs) <= 1 = xs
        | otherwise = merge (mergesort ls) (mergesort rs)
        where
            ls = take ((length xs) `div` 2) xs
            rs = drop ((length xs) `div` 2) xs

remove_ao :: String -> String
remove_ao input = map (\c -> if c=='a' || c=='o' then '_' else c) input

reversePairList :: [(a, b)] -> [(b, a)]
reversePairList list = reverse [(b, a) | (a, b) <- list]

remove_01 :: [(Int, Int)] -> [(Int, Int)]
remove_01 list = filter (\(first, second) -> first /= 0 && second /= 1) list

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x:(removeDuplicates xs)
    
mergesort_let :: (Ord a) => [a] -> [a]
mergesort_let xs 
        | (length xs) <= 1 = xs
        | otherwise = 
            let
                ls = take ((length xs) `div` 2) xs
                rs = drop ((length xs) `div` 2) xs
            in merge (mergesort_let ls) (mergesort_let rs)
