module AufgabeC where

import System.Random (randomRIO)

data Strategy = Bad | Nice
    deriving (Eq)

data Player = Player {name::String, location::Int, target::Int}
instance Show Player where
    show (Player n l t) = n ++ " (" ++ show l ++ ", " ++ show t ++ ")"

setLocation :: Player -> Int -> Player
setLocation (Player n l t) nl = Player n nl t

data State = InProgress {currentPlayer::Player, otherPlayer::Player} | GameOver {winner::Player}
instance Show State where
    show (InProgress cp op) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"

start :: State
start = InProgress (Player "A" 1 1) (Player "B" 8 8)

nextPos _ s@(GameOver _) _ = s
nextPos strategy (InProgress cp@(Player _ cl ct) op@(Player _ ol ot)) d
    | ct /= ol = InProgress op (setLocation cp nl)
    | strategy == Bad = GameOver cp
    | otherwise = InProgress op (setLocation cp (onField nl - 1))
        where 
            nl = onField (cl + d)

turn :: IO State -> IO State
turn sio = do
    d <- rollDice
    os <- sio
    let cpName = name . currentPlayer $ os
    putStr $ cpName ++ " rolled " ++ show d ++ "\n"
    let ns = nextPos Nice os d
    putStr $ show ns ++ "\n"
    return ns

firstTurn = turn . pure $ start

rollDice :: IO Int
rollDice = randomRIO (1, 6)

onField :: Int -> Int
onField = flip mod 24


-- ludoInteractive :: IO()
-- ludoInteractive = do
 