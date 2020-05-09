-- Infos

-- Juri Lozowoj, 35244015
-- Jan Müller, 35011918

-- Aufgabe a

someFunction :: [Integer]
someFunction = replicate 2 . product . map (*3) $ zipWith max [1, 2] [4, 5]

-- Aufgabe b

-- myConcat ["abc","def","g"]
myConcat :: [String] -> String
myConcat = foldl (++) []

myElem :: (Foldable t, Eq a) => a -> t a -> Bool 
myElem x = foldl check False
    where check found y = found || x == y

-- Aufgabe c

-- countElem 1 [1, 2, 3, 1, 4, 1, 5, 6, 1, 7]
countElem :: (Foldable t, Eq a) => a -> t a -> Integer
countElem x = foldr count 0
    where count y acc = acc + if x == y then 1 else 0

-- myId [1, 3, 5, 7, 9, 8, 6, 4, 2, 0]
myId :: [a] -> [a]
myId = foldr (:) []

-- Aufgabe d

-- insertionSort :: (Ord a) => [a] -> [a]
-- insertionSort = foldl insert []
--     where 
--         insert [] y = [y]
--         insert (x:xs) y = if y <= x then y:x:xs else x : insert xs y

-- insertionSort [6, 1, 8, 2, 5, 2, 3]
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldl insert []
    where 
        insert sorted x = foldr shiftLeft [x] sorted
        shiftLeft y (z:zs) = if y > z then z:y:zs else y:z:zs

-- one2two [1,3,2,4,5,6]
one2two :: [a] -> ([a], [a])
one2two = foldr dist ([], [])
    where dist y (a, b) = (y:b, a)

-- Aufgabe e

-- getIndex ['a','k','b']
getIndex :: [a] -> [(Int, a)]
getIndex = reverse . foldl indexify []
    where indexify ys y = (length ys, y):ys

-- removeTwins [(False, False), (False, True), (True, False), (True, True)]
removeTwins :: (Eq a) => [(a, a)] -> [(a, a)]
removeTwins = foldr remove []
    where remove (a, b) acc = if a /= b then (a, b):acc else acc

-- perms [2,3,5]
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [y | x <- xs, y <- map (x:) . perms $ remove x xs]
    where 
        remove x = reverse . snd . foldl (condAdd x) (False, [])
        condAdd x (cond, ys) y = if not cond && y == x then (True, ys) else (cond, y:ys)
