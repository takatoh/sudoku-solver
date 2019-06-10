module Main where

import System.Environment ( getArgs )


data Cell = Confirmed Int
          | Perhaps [Int]
          deriving (Show, Eq)

type Board = [Cell]


main :: IO ()
main = do argv <- getArgs
          cs <- readFile $ head argv
          let board = parseBoard cs
          printBoard board


parseBoard :: String -> Board
parseBoard = undefined


printBoard :: Board -> IO ()
printBoard = undefined
