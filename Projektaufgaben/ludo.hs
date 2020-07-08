-- Requires System.Random to be installed
-- Compilation: stack ghc ludo.hs 
-- Execution: ludo [statisticRoundCount]
-- GHCi: stack ghci ludo.hs
    -- Usage: ludoInteractive OR ludoStatistic roundCount
    -- ludoStatistic 1000

import Control.Exception (evaluate)
import Control.DeepSeq (force)
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
    when ii $ printTurn d
    if isOnField cp && isOnField op && onField (d + justLocation cp) == justLocation op
        then applyStrat s d <$> getStrat cp -- new location is same field as opponent
        else return $! InProgress ii op (move cp d) -- just move. return strictly, because the result will be inspected anyway
    where
        isOnField p = case location p of 
            Nothing -> False
            _ -> True
        justLocation p = fromJust $ location p
        printTurn d = putStr $ show s ++ "\n" ++ name cp ++ " has rolled a " ++ show d ++ ".\n\n"

playRound :: State -> IO State
playRound s = (\ns@(InProgress _ _ p) -> if stepsTaken p < fieldSize then playRound ns else return $ GameOver p) =<< turn s

playRounds :: State -> Int -> IO (Int, Int)
playRounds s = (!!) $ iterate (creditWinner $ playRound s) $ pure (0, 0)   
    where creditWinner r sc = do
            w <- winner <$> r
            (aw, bw) <- sc
            let nsc = if name w == name (currentPlayer s) then (aw + 1, bw) else (aw, bw + 1)
            evaluate $ force nsc -- force evaluation to prevent long chains of additions

ludoInteractive :: IO ()
ludoInteractive = print =<< playRound (InProgress True (Player "A" 1 Nothing Nothing 0) $ Player "B" 8 (Just Bad) Nothing 0)

ludoStatistic :: Int -> IO ()
ludoStatistic rc = do
    let playerA = Player "A" 1 (Just Bad) Nothing 0 
    let playerB = Player "B" 8 (Just Nice) Nothing 0 
    (aw, bw) <- playRounds (InProgress False playerA playerB) rc
    printWins playerA aw
    printWins playerB bw
        where printWins p w = putStr $ name p ++ " has won " ++ show w ++ " round(s) using the " ++ (show . fromJust . strat $ p) ++ " strategy.\n"

main :: IO ()
main = (\args -> if null args then ludoInteractive else ludoStatistic . read $ head args) =<< getArgs
