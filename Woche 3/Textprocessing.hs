module Textprocessing (histogram) where

import Data.List (sort)

-- histogram "hallihallo"
histogram :: String -> [(Char, Int)]
histogram xs = map counted . sort $ distinct xs
    where counted x = (x, countElem x xs)

countElem :: Eq a => a -> [a] -> Int
countElem x = foldr (\y count -> if x == y then count + 1 else count) 0

distinct :: Eq a => [a] -> [a]
distinct = foldl (\xs x -> if x `elem` xs then xs else x:xs) []
