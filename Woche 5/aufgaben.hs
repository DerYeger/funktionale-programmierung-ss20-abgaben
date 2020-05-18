-- Infos

module FP05 where

import Data.List(foldl')

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
            SimpleTime {offset :: String, hour :: Int}

-- ExactTime 20 45 :: Time
-- ExactTime 11 30 :: Time
-- ExactTime 14 00
-- SimpleTime "halb" 12 :: Time
-- SimpleTime "um" 7
instance Show Time where 
    show (ExactTime hour minute)
        | minute == 0 = show $ SimpleTime "um" hour
        | minute == 30 = show $ SimpleTime "halb" (hour + 1)
        | otherwise = show hour ++ ":" ++ formatMinute minute
            where formatMinute x = (if x <= 9 then "0" else "") ++ show x
    show (SimpleTime offset hour) = offset ++ " " ++ formatHour hour
        where formatHour x = show $ if x == 12 then x else x `mod` 12

-- Aufgabe f

-- Aufgabe g

-- Aufgabe h

-- Aufgabe i
