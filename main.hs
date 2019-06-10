module Main where

import System.Environment ( getArgs )


data Cell = Confirmed Int
          | Perhaps [Int]
          deriving (Show, Eq)

type Board = [Cell]


main :: IO ()
main = do argv <- getArgs
          let board = loadBoard $ head argv
          printBoard board


loadBoard :: String -> Board
loadBoard = undefined


printBoard :: Board -> IO ()
printBoard = undefined
