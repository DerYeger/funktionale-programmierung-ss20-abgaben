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

-- Aufgabe f

-- Aufgabe g

-- Aufgabe h

-- Aufgabe i
