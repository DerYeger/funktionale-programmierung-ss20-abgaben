-- Infos

-- Juri Lozowoj, 35244015
-- Jan Müller, 35011918

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

instance (Show a) => Show (Tree a) where
    show = showInset 0
        where 
            showInset i Empty = replicate i ' ' ++ "Empty"
            showInset i (Node l m r) = nextIns r ++ "\n" ++ replicate i ' ' ++ show m ++ "\n" ++ nextIns l
                where nextIns = showInset (i + 5)

-- Aufgabe d
