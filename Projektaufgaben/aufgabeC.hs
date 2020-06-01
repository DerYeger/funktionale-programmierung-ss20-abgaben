module AufgabeC (ludoInteractive, ludoStatistic) where 

import System.Random (randomRIO)
import Data.Maybe (fromJust)
import Control.Monad (when)

fieldSize :: Int
fieldSize = 24

onField :: Int -> Int
onField x
    | x > fieldSize = x `mod` fieldSize
    | x == 0 = 1
    | otherwise = x

data Strategy = Bad | Nice
    deriving (Eq, Show)

data Player = Player {name::String, origin:: Int, location::Int, remaining::Int, strat:: Maybe Strategy}
instance Show Player where
    show (Player n _ l r _) = n ++ " is on " ++ show l ++ " and has " ++ show r ++ " steps left."


data State = InProgress {currentPlayer::Player, otherPlayer::Player, isInteractive::Bool} | GameOver {winner::Player}
instance Show State where
    show (InProgress cp op _) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"
    show (GameOver w) = "Player "++ name w ++ " has won the game.\n"

toOrigin :: Player -> Player
toOrigin (Player n o l r s) = Player n o o fieldSize s

move :: Player -> Int -> Player
move (Player n o l r s) nl = Player n o nl (r - rd) s
    where rd
            | l == nl = 0 -- didnt move
            | nl > l = nl - l -- move didnt pass fieldSize
            | otherwise = fieldSize - l + nl -- move passed fieldSize

applyStrat :: State -> Strategy -> State
applyStrat (InProgress cp op ii) Nice = InProgress op (move cp (onField (location op - 1))) ii
applyStrat (InProgress cp op ii) Bad = InProgress (toOrigin op) (move cp (location op)) ii

getStrat :: Player -> IO Strategy
getStrat (Player _ _ _ _ (Just strat)) = return strat
getStrat _ = do
    putStr "Select your strategy (Bad == Bad, Otherwise == Nice)\n"
    strat <- getLine
    putStr "\n"
    if strat == "Bad" then return Bad else return Nice

nextPos :: State -> IO State
nextPos s@(GameOver _) = return s
nextPos s@(InProgress cp op ii) = do
    d <- randomRIO (1, 6)
    printTurn s d
    let nl = onField (location cp + d)
    if nl == location op
        then applyStrat s <$> getStrat cp -- new location is same field as opponent
        else return $ InProgress op (move cp nl) ii -- just move

checkWinner :: State -> State
checkWinner s@(GameOver _) = s
checkWinner s@(InProgress _ p _)
    | remaining p > 0 = s
    | otherwise = GameOver p

playRound :: IO State -> IO State
playRound sio = do
    os <- sio
    ns <- checkWinner <$> nextPos os
    case checkWinner ns of 
        GameOver{} -> pure ns -- game is over 
        InProgress{} -> playRound (pure ns) -- continue with next turn

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

ludoInteractive :: IO State
ludoInteractive = playRound (pure $ InProgress (Player "A" 1 1 fieldSize Nothing) (Player "B" 8 8 fieldSize (Just Bad)) True)

printTurn :: State -> Int -> IO ()
printTurn s@(InProgress cp _ isInteractive) d = 
    when isInteractive . putStr $ show s ++ name cp ++ " has rolled a " ++ show d ++ ".\n\n" 
