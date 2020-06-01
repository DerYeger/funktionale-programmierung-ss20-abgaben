module AufgabeC where

import System.Random (randomRIO)

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

data State = InProgress {currentPlayer::Player, otherPlayer::Player} | GameOver {winner::Player}
instance Show State where
    show (InProgress cp op) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"
    show (GameOver w) = show (name w) ++ " has won the game.\n"

start :: State
start = InProgress (Player "A" 1 1 fieldSize (Just Bad)) (Player "B" 8 8 fieldSize (Just Nice))

applyStrat :: State -> Strategy -> State
applyStrat (InProgress cp op) Nice = InProgress op (move cp (onField (location op) - 1))
applyStrat (InProgress cp op) Bad = InProgress (toOrigin op) (move cp (location op))

nextPos :: State -> IO State
nextPos s@(GameOver _) = return s
nextPos s@(InProgress cp@(Player _ _ cl _ _) op@(Player _ _ ol _ _)) = do
    d <- randomRIO (1, 6)
    let nl = onField (cl + d)
    if nl == ol
        then applyStrat s <$> getStrat cp
        else return $ InProgress op (move cp nl) -- just move

toOrigin (Player n o l r s) = Player n o o fieldSize s

checkWinner :: State -> State
checkWinner s@(GameOver _) = s
checkWinner s@(InProgress _ p)
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
        (InProgress _ _) -> playRound (pure ns)

automatedRounds :: Int -> IO (Int, Int)
automatedRounds rc =
    if rc <= 0 
        then return(0,0) 
        else do
            (aw, bw) <- automatedRounds $ rc - 1
            w <- winner <$> playRound (pure start)
            if name w == "A" then return (aw + 1, bw) else return (aw, bw + 1)

ludoStatistic :: Int -> IO ()
ludoStatistic rc = do
    (aw, bw) <- automatedRounds rc
    putStr $ "A has won " ++ show aw ++ " round(s) using the " ++ show Bad ++ " strategy.\n"
        ++ "B has won " ++ show bw ++ " round(s) using the " ++ show Nice ++ " strategy.\n"

getStrat :: Player -> IO Strategy
getStrat (Player _ _ _ _ (Just s)) = return s
getStrat _ = do
    putStr "Select your strategy (Bad, Nice)\n"
    s <- getLine
    if s == "Bad" then return Bad else return Nice
