module AufgabeC where

import System.Random (randomRIO)

data Strategy = Bad | Nice
    deriving (Eq)

data Player = A | B
    deriving (Show)

type Pos = (Int, Player) 
type State = (Pos, Pos)

start :: State
start = ((1, A), (8, B))

nextPos strategy ((al, ap), (pl , pp)) d
    | trgt /= pl = ((pl, pp), (trgt, ap))
    | strategy == Bad = ((0, pp), (trgt, ap))
    | otherwise = ((pl, pp), (trgt - 1, ap))
        where 
            trgt = next (al + d)

turn :: IO State -> IO State
turn state = do
    d <- rollDice
    os <- state
    putStr $ show (snd . fst $ os) ++ " rolled " ++ show d ++ "\n"
    let ns = nextPos Nice os d
    putStr $ show ns ++ "\n\n"
    pure ns

firstTurn = turn . pure $ start

rollDice :: IO Int
rollDice = randomRIO (1, 6)

next :: Int -> Int
next = flip mod 24