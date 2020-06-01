module AufgabeC where

import System.Random (randomRIO)
import Data.Maybe (fromJust)

data Strategy = Bad | Nice
    deriving (Eq, Show)

fieldSize :: Int
fieldSize = 24

data Player = Player {name::String, origin:: Int, location::Int, remaining::Int, strat:: Maybe Strategy}
instance Show Player where
    show (Player n _ l r _) = n ++ " is on " ++ show l ++ " and has " ++ show r ++ " steps left."

move :: Player -> Int -> Player
move (Player n o l r s) nl = Player n o nl (r - rd) s
    where 
        rd
            | l == nl = 0 -- didnt move
            | nl > l = nl - l -- move didnt pass 24
            | otherwise = fieldSize - l + nl -- move passed 24

data State = InProgress {currentPlayer::Player, otherPlayer::Player, isInteractive::Bool} | GameOver {winner::Player}
instance Show State where
    show (InProgress cp op _) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"
    show (GameOver w) = "Player "++ name w ++ " has won the game.\n"

applyStrat :: State -> Strategy -> State
applyStrat (InProgress cp op ii) Nice = InProgress op (move cp (onField (location op) - 1)) ii
applyStrat (InProgress cp op ii) Bad = InProgress (toOrigin op) (move cp (location op)) ii

nextPos :: State -> IO State
nextPos s@(GameOver _) = return s
nextPos s@(InProgress cp@(Player _ _ cl _ _) op@(Player _ _ ol _ _) ii) = do
    d <- randomRIO (1, 6)
    printState s d
    let nl = onField (cl + d)
    if nl == ol
        then applyStrat s <$> getStrat cp
        else return $ InProgress op (move cp nl) ii -- just move

toOrigin :: Player -> Player
toOrigin (Player n o l r s) = Player n o o fieldSize s

checkWinner :: State -> State
checkWinner s@(GameOver _) = s
checkWinner s@(InProgress _ p _)
    | remaining p > 0 = s
    | otherwise = GameOver p

onField :: Int -> Int
onField x
    | x > fieldSize = x `mod` fieldSize
    | x == 0 = 1
    | otherwise = x

playRound :: IO State -> IO State
playRound sio = do
    os <- sio
    ns <- checkWinner <$> nextPos os
    case checkWinner ns of 
        (GameOver _) -> pure ns
        InProgress{} -> playRound (pure ns)

automatedRounds :: Int -> State -> IO (Int, Int)
automatedRounds rc start =
    if rc <= 0 
        then return(0,0) 
        else do
            (aw, bw) <- automatedRounds (rc - 1) start
            w <- winner <$> playRound (pure start)
            if name w == name (currentPlayer start) then return (aw + 1, bw) else return (aw, bw + 1)

ludoStatistic :: Int -> IO ()
ludoStatistic rc = do
    let playerA = Player "A" 1 1 fieldSize (Just Bad)
    let playerB = Player "B" 8 8 fieldSize (Just Nice)
    (aw, bw) <- automatedRounds rc (InProgress playerA playerB False)
    putStr $ "A has won " ++ show aw ++ " round(s) using the " ++ show (fromJust (strat playerA)) ++ " strategy.\n"
        ++ "B has won " ++ show bw ++ " round(s) using the " ++ show (fromJust (strat playerB)) ++ " strategy.\n"

getStrat :: Player -> IO Strategy
getStrat (Player _ _ _ _ (Just strat)) = return strat
getStrat _ = do
    putStr "Select your strategy (Bad == Bad, Otherwise == Nice)\n"
    strat <- getLine
    if strat == "Bad" then return Bad else return Nice

ludoInteractive :: IO State
ludoInteractive = playRound (pure $ InProgress (Player "A" 1 1 fieldSize Nothing) (Player "B" 8 8 fieldSize (Just Bad)) True)

printState :: State -> Int -> IO ()
printState s@(InProgress cp _ isInteractive) d = 
    if isInteractive
        then putStr $ show s ++ name cp ++ " has rolled a " ++ show d ++ ".\n\n" 
        else return()
