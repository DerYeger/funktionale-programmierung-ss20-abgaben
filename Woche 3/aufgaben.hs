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

-- TODO insertionSort

-- one2two [1,3,2,4,5,6]
one2two :: [a] -> ([a], [a])
one2two = foldr split ([], [])
    where split y (a, b) = (y:b, a)

-- Aufgabe e

-- getIndex ['a','k','b']
getIndex :: [a] -> [(Integer, a)]
getIndex = reverse . snd . foldl indexify (0, [])
    where indexify (index, ys) y = (index + 1, (index, y):ys)

-- removeTwins [(False, False), (False, True), (True, False), (True, True)]
removeTwins :: (Eq a) => [(a, a)] -> [(a, a)]
removeTwins = foldr remove []
    where remove (a, b) acc = if a /= b then (a, b) : acc else acc

-- TODO perms
