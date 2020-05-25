-- Infos

-- Juri Lozowoj, 35244015
-- Jan Müller, 35011918

module FP06 where

import Data.List (sort)

-- Aufgabe a

newtype Keller a = Keller {derKeller :: ([a], Maybe a)}

instance (Show a) => Show (Keller a) where
    show (Keller (t, b)) = "Keller: " ++ show t ++ ", oben: " ++ show b

neuerKeller = Keller ([], Nothing)

-- Die Funktionen pushd und popd aus Abschnitt 4.2.4 lassen sich wie folgt vereinfachen:

pushd :: Keller a -> a -> Keller a
pushd (Keller (stk, _)) y = Keller (y:stk, Just y)

popd :: (Eq a) => Keller a -> Keller a
popd (Keller (x:y:ys, _)) = Keller (y:ys, Just y)
popd (Keller _) = Keller ([], Nothing)

-- Aufgabe b

-- Beim zweiten Beispiel wird auf Ungleicheit geprüft.
-- Dabei handelt es sich um die Negation der Gleichheit, welche durch Zugehörigkeit der Klasse Eq definiert wurde.
-- Dementsprechend ist es nicht notwendig die Negation einzeln zu definieren.
-- Sie wird aus der existierenden Definition abgeleitet.

-- Aufgabe c

data Tree a = Empty | Node (Tree a) a (Tree a)

simpleTree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
exampleTree = Node (Node Empty 11 Empty) 10 (Node (Node Empty 9 Empty) 7 (Node Empty 5 Empty))

-- getRoot simpleTree 
-- getRoot exampleTree
-- getRoot Empty
getRoot :: Tree a -> Maybe a
getRoot (Node _ m _) = Just m
getRoot _ = Nothing

instance (Show a) => Show (Tree a) where
    show t =  "\n" ++ showInset 0 t ++ "\n"
        where 
            showInset i Empty = replicate i ' ' ++ "Empty"
            showInset i (Node l m r) = nextIns r ++ "\n" ++ replicate i ' ' ++ show m ++ "\n" ++ nextIns l
                where nextIns = showInset (i + 5)

-- computeSum simpleTree 
-- computeSum exampleTree
-- computeSum Empty
computeSum :: (Num a) => Tree a -> a
computeSum Empty = 0
computeSum (Node l m r) = computeSum l + m + computeSum r

-- fmap (*2) simpleTree
-- fmap (*2) exampleTree
-- fmap (*2) Empty
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node l m r) = Node (fmap f l) (f m) (fmap f r)

countNodes :: Tree a -> Int
countNodes (Node l m r) = 1 + countNodes l + countNodes r
countNodes Empty = 0

instance Eq (Tree a) where
    fst == snd = countNodes fst == countNodes snd         

-- compare Empty (Node Empty 3 Empty)
-- (Node Empty 3 Empty) == (Node Empty 5 Empty)
-- sort [exampleTree, simpleTree, Empty]
instance Ord (Tree a) where
    fst `compare` snd = countNodes fst `compare` countNodes snd

-- ti1 = insert (2, "zwei") Empty
-- ti2 = insert (1, "eins") ti1
-- ti3 = insert (4, "vier") ti2
-- ti4 = insert (3, "drei") ti3
-- ti5 = insert (4, "nochEineVier") ti4
insert :: (Ord a) => (a, b) -> Tree (a, b) -> Tree (a, b)
insert n Empty = Node Empty n Empty
insert n@(nk, _) (Node l m@(mk, _) r)
    | nk < mk = Node l m (insert n r)
    | nk == mk = Node l m r
    | otherwise = Node (insert n l) m r

-- Aufgabe d
