-- Requires System.Random to be installed
-- Compilation: stack ghc ludo.hs 
-- Execution: ludo [statisticRoundCount]
-- GHCi: stack ghci ludo.hs
    -- Usage: ludoInteractive OR ludoStatistic roundCount
    -- ludoStatistic 1000

import Control.Monad (when, void)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Random (randomRIO)

fieldSize :: Int
fieldSize = 24

onField :: Int -> Int
onField x
    | x > fieldSize = x `mod` fieldSize
    | x == 0 = 1
    | otherwise = x

data Strategy = Bad | Nice
    deriving (Eq, Show)

data Player = Player {name::String, origin::Int, strat::Maybe Strategy, location::Maybe Int, stepsTaken::Int}
instance Show Player where
    show (Player n _ _ l st) = n ++ " is on " ++ show l ++ " and has " ++ show (fieldSize - st)++ " steps left."


data State = InProgress {isInteractive::Bool, currentPlayer::Player, otherPlayer::Player} | GameOver {winner::Player}
instance Show State where
    show (InProgress _ cp op) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"
    show (GameOver w) = "Player "++ name w ++ " has won the game.\n"

move :: Player -> Int -> Player
move (Player n o s Nothing st) d = Player n o s (Just $ o + d - 1) $ d - 1
move (Player n o s (Just l) st) d = Player n o s (Just . onField $ l + d) $ st + d

applyStrat :: State -> Int -> Strategy -> State
applyStrat (InProgress ii cp op) d strat = case strat of
    Nice -> InProgress ii op $ move cp $ d - 1
    Bad -> InProgress ii (toOrigin op) $ move cp d
        where toOrigin (Player n o s _ _ ) = Player n o s Nothing 0

getStrat :: Player -> IO Strategy
getStrat (Player _ _ (Just s) _ _) = return s
getStrat _ = do
    putStr "Select your strategy (Bad == Bad, Otherwise == Nice)\n"
    s <- getLine
    putStr "\n"
    if s == "Bad" then return Bad else return Nice

turn :: State -> IO State
turn s@(GameOver _) = return s
turn s@(InProgress ii cp op) = do
    d <- randomRIO (1, 6)
    when ii $ printTurn s d
    if isOnField cp && isOnField op && onField (d + justLocation cp) == justLocation op
        then applyStrat s d <$> getStrat cp -- new location is same field as opponent
        else return $! InProgress ii op (move cp d) -- just move. return strictly, because the result will be inspected anyway
    where
        isOnField p = case location p of 
            Nothing -> False
            _ -> True
        justLocation p = fromJust $ location p
        printTurn s@(InProgress _ cp _) d = putStr $ show s ++ "\n" ++ name cp ++ " has rolled a " ++ show d ++ ".\n\n"

playRound :: State -> IO State
playRound s = checkGameOver <$> turn s >>= \ns -> case ns of
    GameOver{} -> return ns -- game is over 
    InProgress{} -> playRound ns -- continue with next turn
    where 
        checkGameOver s@(GameOver _) = s
        checkGameOver s@(InProgress _ _ p)
            | stepsTaken p < fieldSize = s
            | otherwise = GameOver p

playRounds :: Int -> State -> IO (Int, Int)
playRounds rc start =
    if rc <= 0 
        then return (0,0) 
        else do
            (aw, bw) <- playRounds (rc - 1) start
            w <- winner <$> playRound start
            if name w == name (currentPlayer start) then return $! (aw + 1, bw) else return $! (aw, bw + 1) -- increment strictly to prevent long chains of additions

ludoInteractive :: IO ()
ludoInteractive = print =<< playRound (InProgress True (Player "A" 1 Nothing Nothing 0) (Player "B" 8 (Just Bad) Nothing 0))

ludoStatistic :: Int -> IO ()
ludoStatistic rc = do
    let playerA = Player "A" 1 (Just Bad) Nothing 0 
    let playerB = Player "B" 8 (Just Nice) Nothing 0 
    (aw, bw) <- playRounds rc (InProgress False playerA playerB)
    printWins playerA aw
    printWins playerB bw
        where printWins p w = putStr $ name p ++ " has won " ++ show w ++ " round(s) using the " ++ (show . fromJust . strat $ p) ++ " strategy.\n"

main :: IO ()
main = do
    args <- getArgs
    if null args
        then ludoInteractive
        else ludoStatistic (read (head args) :: Int)