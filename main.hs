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
          printBoard board


parseBoard :: String -> Board
parseBoard cs = map stringToCells $ lines cs


stringToCells :: String -> [Cell]
stringToCells s = map charToCell s


charToCell :: Char -> Cell
charToCell ' ' = Perhaps [1, 2, 3, 4, 5, 6, 7, 8, 9]
charToCell c   = Confirmed (ord c - 48)


printBoard :: Board -> IO ()
printBoard = undefined
