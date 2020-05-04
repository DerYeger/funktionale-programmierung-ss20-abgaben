-- Infos

-- Juri Lozowoj, 35244015
-- Jan Müller, 35011918

-- Aufgabe a

someFunction :: [Integer]
someFunction = replicate 2 . product . map (*3) $ zipWith max [1, 2] [4, 5]

-- Aufgabe b

myConcat :: [String] -> String
myConcat = foldl (++) []
