{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import System.IO
import Data.Array
import Data.Char
import Data.List.Split

n :: Int
n = 8

type Field = Array (Int, Int) Char
type Row = Array Int Char 
type Index = (Int, Int)
data Player = White | Black
data Status = Running | Endgame (Maybe Player)
data State = State { field :: Field, player :: Player, status :: Status }

instance {-# OVERLAPPING #-} Show Row where
    show r = foldr (:) "" r

instance {-# OVERLAPPING #-} Show Field where
    show f = (concat showRows) ++ letters where
        showRows = showRow <$> zip (reverse rows) [n,n-1..1]
        letters = "  " ++ (take n $ iterate succ 'a') ++ "\n"
        rows = [getRow f i | i <- [0..n-1]]
        showRow (r, i) = show i ++ " " ++ show r ++ "\n"

instance Show Player where
    show White = "White"
    show Black = "Black"

instance Show Status where
    show Running = "Running"
    show (Endgame Nothing) = "DRAW"
    show (Endgame (Just x)) = show x ++ " WON!"

instance Show State where
    show (State field player status) = case status of
        Running -> "\n" ++ show status ++ "\n" ++  show field ++ show player ++ " to play:"
        _       -> "\n" ++ show status ++ "\n" ++  show field

instance Eq Player where
    (==) White White = True
    (==) Black Black = True
    (==) _     _     = False

instance {-# OVERLAPPING #-} Semigroup Index where
    (x, y) <> (x', y') = (x + x', y + y')

playerToLowercase :: Player -> Char
playerToLowercase White = 'w'
playerToLowercase Black = 'b'

playerToUppercase :: Player -> Char
playerToUppercase White = 'W'
playerToUppercase Black = 'B'

changePlayer :: Player -> Player
changePlayer White = Black
changePlayer Black = White

parseState :: String -> State
parseState s = State field player status where
    rows = splitOn "\n" s
    player = case head rows of 
        "White" -> White
        "Black" -> Black
    field = parseField $ tail $ reverse $ tail rows
    parseField rws = array ((0, 0), (n - 1, n - 1)) [((i, j), (rws !! j) !! i) | i <- [0..n-1], j <- [0..n-1]]
    status = Running -- ADD CHECK ENDGAME

getRow :: Field -> Int -> Row
getRow f i = array (0, n - 1) [(j, f ! (j, i)) | j <- [0..n-1]]

countPieces :: State -> (Int, Int)
countPieces (State field _ _) = (count 'w' field, count 'b' field) where
    count char field = sum $ fmap (\c -> if c == char || c == toUpper char then 1 else 0) field -- Можно лучше --- за один проход!

checkPlayerAndSquare :: Player -> Field -> Index -> Bool
checkPlayerAndSquare White field index = toUpper (field ! index) == 'W'
checkPlayerAndSquare Black field index = toUpper (field ! index) == 'B'

isKing :: Field -> Index -> Bool
isKing field index = isUpper $ field ! index

isMan :: Field -> Index -> Bool
isMan field index = isLower $ field ! index

isEmpty :: Field -> Index -> Bool
isEmpty field index = '.' == field ! index

manMove :: Player -> Field -> Index -> Char -> Maybe (Field, Index)
manMove player field index c = let delta = (deltaX, deltaY)
                                   deltaX = if player == White then 1 else -1
                                   deltaY = if c == '<' then -1 else 1
                                   playerLowercased = playerToLowercase player
    in case field ! (index <> delta) of
        '.' -> Just (field // [(index, '.'), (index <> delta, playerToLowercase player)], index <> delta)
        c -> Nothing -- TODO eating

kingMove :: Player -> Field -> Index -> Char -> Maybe (Field, Index)
kingMove player field index c = Just (field, index)

parseMove :: String -> Maybe (Index, String)
parseMove (x : y : _ : tail) = case 0 <= xc && xc < n && 0 <= yc && yc < n of
            True -> Just ((xc, yc), tail)
            False -> Nothing
        where xc = ord x - ord 'a'
              yc = ord y - ord '1'
parseMove _ = Nothing

iterationUpdateField :: Player -> Maybe (Field, Index) -> Char -> Maybe (Field, Index)
iterationUpdateField _       Nothing               _ = Nothing
iterationUpdateField player (Just (field, index)) c = case checkPlayerAndSquare player field index of
    True  -> case isMan field index of
                 True  -> manMove  player field index c
                 False -> kingMove player field index c
    False -> Nothing

updateField :: Player -> Field -> Maybe (Index, String) -> Maybe (Field, Index)
updateField player field (Just (index, move)) = foldl (iterationUpdateField player) (Just (field, index)) move
updateField player field Nothing = Nothing
    
updateState :: String -> State -> State
updateState move (State field player status) = case updateField player field (parseMove move) of
    Nothing             -> State field player status
    Just (field, index) -> State field' player' status' where
        field' = field
        player' = changePlayer player 
        status' = status -- Добавить проверку статуса!!!

iteration :: State -> IO ()
iteration state = do 
                      putStrLn $ show state
                      case status state of
                          Running -> continue
                          _       -> return ()
    where continue = do
                         move <- getLine
                         iteration $ updateState move state

main :: IO ()
main = do
    strState <- readFile "initialField.txt"
    let state = parseState strState
    iteration state

