-- Infos

-- Juri Lozowoj, 35244015
-- Jan Müller, 35011918

-- Aufgabe a

-- take 10 powersOfTwo
-- head $ dropWhile (<= 100) powersOfTwo
powersOfTwo :: [Int]
powersOfTwo = iterate (2*) 1

-- Aufgabe b
-- take 5 $ zip c d

-- take 10 a
a :: [Int]
a = 2 : map (\n -> 3 * a!!(n - 1) + 4) [1..]

-- take 10 b
b :: [Int]
b = 5 : 2 : 4 : map (\n -> 2 * b!!(n - 3) - 3 * b!!(n - 1) + 4) [3..]

-- take 10 c
c :: [Int]
c = 2 : 3 : map (\i -> d!!(i - 1) - c!!(i - 2)) [2..]

-- take 10 d
d :: [Int]
d = 1 : map (\i -> d!!(i - 1) * c!!i + 1) [1..]

-- Aufgabe c

-- take 10 firstDefinition
firstDefinition :: [Int]
firstDefinition = 0 : 1 : firstDefinition

-- take 10 secondDefinition
secondDefinition :: [Int]
secondDefinition = map (`mod` 2) [0..]

-- take 10 thirdDefinition
thirdDefinition :: [Int]
thirdDefinition = iterate (\x -> (x + 1) `mod` 2) 0

-- take 10 fourthDefinition
fourthDefinition :: [Int]
fourthDefinition = [0, 1] ++ fourthDefinition

-- take 10 fifthDefinition
fifthDefinition :: [Int]
fifthDefinition = cycle [0, 1]

-- Aufgabe d

-- myCurry fst "Correct" "Wrong"
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry fun a b = fun (a, b)

-- myUncurry (+) (3, 4)
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry fun (a, b) = fun a b

-- Aufgabe e

-- checkBrackets "{[]{}[{}]}"
-- checkBrackets "{[]{}[{[}]]}"
-- checkBrackets "{ab[cd]}e{fg[h * i][j{klm}]}"
-- checkBrackets "{ab[c}]"
checkBrackets :: String -> Bool
checkBrackets xs = let (valid, stack) = foldl check (True, []) xs in valid && null stack
    where
        check (valid, stack) c
            | not valid = (False, []) -- brackets did not match previously -> fail
            | isOpenBracket c = (valid, c:stack) -- c is open bracket and added to the stack
            | isClosedBracket c && null stack = (False, []) -- c is closed bracket but no open bracket is on the stack -> fail
            | isClosedBracket c = (c `closes` head stack, tail stack) -- c is closed bracket and checked against last open bracket
            | otherwise = (valid, stack) -- continue with next char
        isOpenBracket = flip elem ['[', '{']
        isClosedBracket = flip elem [']', '}']
        closes r l = l == '[' && r == ']' || l == '{' && r == '}'
