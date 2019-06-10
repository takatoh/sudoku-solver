module Main where

import System.Environment ( getArgs )
import Data.Char


data Cell = Confirmed Int
          | Perhaps [Int]
          deriving (Show, Eq)

type Board = [[Cell]]


main :: IO ()
main = do argv <- getArgs
          cs <- readFile $ head argv
          let board = parseBoard cs
          case (solv board) of
            Just b  -> printBoard b
            Nothing -> putStrLn "Fialed to solve."


parseBoard :: String -> Board
parseBoard cs = map stringToCells $ lines cs


stringToCells :: String -> [Cell]
stringToCells s = map charToCell s


charToCell :: Char -> Cell
charToCell ' ' = Perhaps [1, 2, 3, 4, 5, 6, 7, 8, 9]
charToCell c   = Confirmed (ord c - 48)


printBoard :: Board -> IO ()
printBoard b = putStr $ unlines $ map cellsToString b


cellsToString :: [Cell] -> String
cellsToString cs = map cellToChar cs


cellToChar :: Cell -> Char
cellToChar (Confirmed x) = chr $ x + 48
cellToChar (Perhaps xs)  = '0'


solv :: Board -> Maybe Board
solv board = case (findParhaps board) of
               Just (Perhaps xs, pos) -> pickUp $ map (solv . replace board pos) xs
               Nothing                -> judge board


findParhaps :: Board -> Maybe (Cell, (Int, Int))
findParhaps = undefined


pickUp :: [Maybe Board] -> Maybe Board
pickUp = undefined


replace :: Board -> (Int, Int) -> Int -> Board
replace = undefined


judge :: Board -> Maybe Board
judge = undefined
