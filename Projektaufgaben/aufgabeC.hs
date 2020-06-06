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

data Player = Player {name::String, origin::Int, location::Maybe Int, stepsTaken::Int, strat::Maybe Strategy}
instance Show Player where
    show (Player n _ l st _) = n ++ " is on " ++ show l ++ " and has " ++ show (fieldSize - st)++ " steps left."


data State = InProgress {currentPlayer::Player, otherPlayer::Player, isInteractive::Bool} | GameOver {winner::Player}
instance Show State where
    show (InProgress cp op _) = "Current Player: " ++ show cp ++ "\nWaiting Player: " ++ show op ++ "\n"
    show (GameOver w) = "Player "++ name w ++ " has won the game.\n"

move :: Player -> Int -> Player
move (Player n o Nothing st s) d = Player n o (Just $ o + d - 1) (d - 1) s
move (Player n o (Just l) st s) d = Player n o (Just . onField $ l + d) (st + d) s

justLocation :: Player -> Int
justLocation p = fromJust (location p)

applyStrat :: State -> Int -> Strategy -> State
applyStrat (InProgress cp op ii) d strat = case strat of
    Nice -> InProgress op (move cp $ d - 1) ii
    Bad -> InProgress (toOrigin op) (move cp d) ii
        where toOrigin (Player n o _ _ s) = Player n o Nothing 0 s

getStrat :: Player -> IO Strategy
getStrat (Player _ _ _ _ (Just strat)) = return strat
getStrat _ = do
    putStr "Select your strategy (Bad == Bad, Otherwise == Nice)\n"
    strat <- getLine
    putStr "\n"
    if strat == "Bad" then return Bad else return Nice

isOnField :: Player -> Bool
isOnField p = case location p of 
    Nothing -> False
    _ -> True

turn :: State -> IO State
turn s@(GameOver _) = return s
turn s@(InProgress cp op ii) = do
    d <- randomRIO (1, 6)
    printTurn s d
    if isOnField cp && isOnField op && onField (d + justLocation cp) == justLocation op
        then applyStrat s d <$> getStrat cp -- new location is same field as opponent
        else return $ InProgress op (move cp d) ii -- just move

checkGameOver :: State -> State
checkGameOver s@(GameOver _) = s
checkGameOver s@(InProgress _ p _)
    | stepsTaken p < fieldSize = s
    | otherwise = GameOver p

playRound :: State -> IO State
playRound s = checkGameOver <$> turn s >>= \ns -> case ns of
    GameOver{} -> return ns -- game is over 
    InProgress{} -> playRound ns -- continue with next turn

playRounds :: Int -> State -> IO (Int, Int)
playRounds rc start =
    if rc <= 0 
        then return (0,0) 
        else do
            (aw, bw) <- playRounds (rc - 1) start
            w <- winner <$> playRound start
            if name w == name (currentPlayer start) then return (aw + 1, bw) else return (aw, bw + 1)

ludoStatistic :: Int -> IO ()
ludoStatistic rc = do
    let playerA = Player "A" 1 Nothing 0 (Just Bad)
    let playerB = Player "B" 8 Nothing 0 (Just Nice)
    let printWins p w = putStr $ name p ++ " has won " ++ show w ++ " round(s) using the " ++ (show . fromJust . strat $ p) ++ " strategy.\n"
    (aw, bw) <- playRounds rc (InProgress playerA playerB False)
    printWins playerA aw
    printWins playerB bw 

ludoInteractive :: IO State
ludoInteractive = playRound $ InProgress (Player "A" 1 Nothing 0 Nothing) (Player "B" 8 Nothing 0 (Just Bad)) True

printTurn :: State -> Int -> IO ()
printTurn s@(InProgress cp _ isInteractive) d = 
    when isInteractive . putStr $ show s ++ "\n" ++ name cp ++ " has rolled a " ++ show d ++ ".\n\n" 
