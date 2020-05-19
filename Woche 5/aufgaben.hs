-- Infos

module FP05 where

import Prelude hiding (Left, Up, Right, Down)
import Data.List (foldl')

-- Juri Lozowoj, 35244015
-- Jan Müller, 35011918

-- Aufgabe a
-- myElem könnte mit foldl' platzeffizienter implementiert werden.

-- Aufgabe b

-- reverseCons [1..9]
reverseCons :: [a] -> [a]
reverseCons = foldl' (flip (:)) []

-- reverseConc [1..9]
reverseConc :: [a] -> [a]
reverseConc = foldl' (flip $ (++) . pure) []

-- Aufgabe c
-- Der Name des Typen lautet "Figur", jedoch wird an zwei Stellen "Shape" verwendet.

-- Aufgabe d

data Person = Person {name :: String, surname :: String} deriving(Show)
data Book = Book {author :: String, title :: String} deriving(Show)

-- getTitle somePerson
-- getTitle someBook
getTitle :: Book -> String
getTitle (Book _ title) = title

somePerson = Person "Fred" "Fuchs"
someBook = Book "Doberkat" "Haskell"

-- Aufgabe e

data Time = ExactTime {hour :: Int, minute :: Int}
            |
            SimpleTime {word :: String, hour :: Int}

-- let t1 = ExactTime 20 45 :: Time
-- let t2 = ExactTime 11 30 :: Time
-- let t3 = ExactTime 14 00
-- let t4 = SimpleTime "halb" 12 :: Time
-- let t5 = SimpleTime "um" 7
instance Show Time where 
    show (ExactTime h m)
        | m == 0 = show $ SimpleTime "um" h
        | m == 30 = show $ SimpleTime "halb" (h + 1)
        | otherwise = show h ++ ":" ++ formatMinute
            where formatMinute = (if m <= 9 then "0" else "") ++ show m
    show (SimpleTime w h) = w ++ " " ++ formatHour
        where formatHour = show $ if h == 12 then h else h `mod` 12

-- Aufgabe f

class HasDistance a where
    dist :: a -> a -> Double

-- dist t2 t3
-- dist t3 t2
-- dist t2 t4
-- dist t1 t5
instance HasDistance Time where
    dist fst snd = fromIntegral . abs $ getAbs fst - getAbs snd
        where 
            getAbs (ExactTime h m) = h * 60 + m
            getAbs (SimpleTime w h) = h * 60 - if w == "halb" then 30 else 0

-- Aufgabe g

-- getSimple t1
-- getSimple t2
-- getSimple t3
-- getSimple t4
-- getSimple t5
getSimple :: Time -> Maybe Time
getSimple it@(SimpleTime _ _) = Just it
getSimple (ExactTime h m)
    | m == 0 = Just $ SimpleTime "um" h
    | m == 30 = Just $ SimpleTime "halb" (h + 1)
    | otherwise = Nothing

-- Aufgabe h

data Cell = Cell {yVal :: Int, xVal :: Int}

data Direction = Left | Up | Right | Down

instance Show Cell where 
    show (Cell y x) = "(" ++ show y ++ "," ++ show x ++ ")"

-- move (Cell 4 5) Left
-- move (Cell 5 0) Left
-- move (Cell 9 3) Down
move :: Cell -> Direction -> Cell
move (Cell y x) direction = case direction of 
    Left -> Cell y (lim $ x - 1)
    Up -> Cell (lim $ y - 1) x 
    Right -> Cell y (lim $ x + 1)
    Down -> Cell (lim $ y + 1) x
    where 
        n = 10
        lim x
            | x <= 0 = 0
            | n <= x = n - 1
            | otherwise = x

-- move2 (4,5) Left
-- move2 (5,0) Left
-- move2 (9,3) Down
move2 :: (Int, Int) -> Direction -> Cell
move2 (y, x) = move $ Cell y x

-- Aufgabe i

-- nPerfect 3
nPerfect :: Int -> [Int]
nPerfect = flip take (filter isPerfect [0..])   

isPerfect :: Int -> Bool
isPerfect x = ds + o == 1
    where 
        r = sqrt . fromIntegral $ x
        l = floor r
        subDivs y s
            | s < 1 = 0 -- cancel
            | x `mod` y /= 0 = s -- continue
            | otherwise = s - y - x `div` y -- subtract divisors
        ds = foldr subDivs x [2..l] -- == x - divisors(x) + 1
        o = if ceiling r == l then l else 0 -- if sqrt x is whole number we subtracted one to much

-- Simplere und langsamere Variante
-- nPerfect :: Int -> [Int]
-- nPerfect = flip take perfectNumbers

-- perfectNumbers :: [Int]
-- perfectNumbers = filter isPerfect [1..]
--     where isPerfect x = x == sum [y | y <- [1 .. x `div` 2], x `mod` y == 0 ] 
