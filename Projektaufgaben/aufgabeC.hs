module AufgabeC where

import System.Random (randomRIO)

data Strategy = Bad | Nice
    deriving (Eq)

fieldSize :: Int
fieldSize = 24

data Player = Player {name::String, location::Int, remaining::Int}
instance Show Player where
    show (Player n l r) = n ++ " is on " ++ show l ++ " and has " ++ show r ++ " steps left."

move :: Player -> Int -> Player
move (Player n l r) nl = Player n nl (r - rd)
    where 
        rd
            | l == nl = 0 -- didnt move
            | nl > l = nl - l -- move didnt pass 24
            | otherwise = fieldSize - l + nl -- move passed 24

data State = InProgress {currentPlayer::Player, otherPlayer::Player} | GameOver {winner::Player}
instance Show State where
    show (InProgress cp op) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"
    show (GameOver w) = show (name w) ++ " has won the game.\n"

start :: State
start = InProgress (Player "A" 1 fieldSize) (Player "B" 8 fieldSize)

nextPos _ s@(GameOver _) _ = s
nextPos strategy (InProgress cp@(Player _ cl _) op@(Player _ ol _)) d
    | nl /= ol = InProgress op (move cp nl)
    | strategy == Bad = GameOver cp
    | otherwise = InProgress op (move cp (onField nl - 1))
        where 
            nl = onField (cl + d)

checkWinner :: State -> State
checkWinner s@(GameOver _) = s
checkWinner s@(InProgress _ p)
    | remaining p > 0 = s
    | otherwise = GameOver p

turn :: IO State -> IO State
turn sio = do
    d <- rollDice
    os <- sio
    let cpName = name . currentPlayer $ os
    putStr $ cpName ++ " rolled " ++ show d ++ "\n"
    let ns = checkWinner $ nextPos Nice os d
    putStr $ show ns ++ "\n"
    return ns

firstTurn = turn . pure $ start

rollDice :: IO Int
rollDice = randomRIO (1, 6)

onField :: Int -> Int
onField x
    | x > fieldSize = x `mod` fieldSize
    | x == 0 = 1
    | otherwise = x


-- ludoInteractive :: IO()
-- ludoInteractive = do
 

-- nextPos Nice (InProgress (Player "A" 23 2) (Player "B" 24 8)) 1