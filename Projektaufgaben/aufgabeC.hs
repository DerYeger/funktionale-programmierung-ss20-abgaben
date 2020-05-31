module AufgabeC where

import System.Random (randomRIO)

data Strategy = Bad | Nice
    deriving (Eq, Show)

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
    | nl /= ol = InProgress op (move cp nl) -- just move
    | strategy == Bad = GameOver cp -- TODO Does this actually cause a loss?
    | otherwise = InProgress op (move cp (onField nl - 1)) -- apply nice strategy
        where 
            nl = onField (cl + d)

checkWinner :: State -> State
checkWinner s@(GameOver _) = s
checkWinner s@(InProgress _ p)
    | remaining p > 0 = s
    | otherwise = GameOver p

rollDice :: IO Int
rollDice = randomRIO (1, 6)

onField :: Int -> Int
onField x
    | x > fieldSize = x `mod` fieldSize
    | x == 0 = 1
    | otherwise = x

automatedTurn :: Strategy -> IO State -> IO State
automatedTurn strat sio = do
    d <- rollDice
    os <- sio
    let cpName = name . currentPlayer $ os
    -- putStr $ cpName ++ " rolled " ++ show d ++ " and uses the " ++ show strat ++ " strategy.\n"
    let ns = checkWinner $ nextPos strat os d
    -- putStr $ show ns ++ "\n"
    case ns of 
        (GameOver _) -> pure ns
        (InProgress _ _) -> automatedTurn (nextStrat strat) (pure ns)

automatedRounds :: Int -> IO (Int, Int)
automatedRounds rc =
    if rc <= 0 
        then return(0,0) 
        else do
            (aw, bw) <- automatedRounds $ rc - 1
            r <- automatedTurn Bad (pure start)
            let w =  winner r
            let new = if name w == "A" then (aw + 1, bw) else (aw, bw + 1)
            pure new

ludoStatistic :: Int -> IO ()
ludoStatistic rc = do
    (aw, bw) <- automatedRounds rc
    putStr $ "A has won " ++ show aw ++ " round(s) using the " ++ show Bad ++ " strategy.\n"
        ++ "B has won " ++ show bw ++ " round(s) using the " ++ show Nice ++ " strategy.\n"

nextStrat :: Strategy -> Strategy
nextStrat Bad = Nice
nextStrat Nice = Bad
