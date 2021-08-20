module Lib where

import Data.Char
import Data.List

--------------------------------------------------------------------------------

data Cell = Confirmed Int
          | Perhaps [Int]
          deriving (Show, Eq)


type Board = [[Cell]]

--------------------------------------------------------------------------------

parseBoard :: String -> Board
parseBoard = map stringToCells . lines


stringToCells :: String -> [Cell]
stringToCells = map charToCell


charToCell :: Char -> Cell
charToCell ' ' = Perhaps [1..9]
charToCell c   = Confirmed (ord c - 48)


printBoard :: Board -> IO ()
printBoard = putStr . unlines . map cellsToString


cellsToString :: [Cell] -> String
cellsToString = map cellToChar


cellToChar :: Cell -> Char
cellToChar (Confirmed x) = chr $ x + 48
cellToChar (Perhaps xs)  = ' '

--------------------------------------------------------------------------------

solv :: Board -> Maybe Board
solv board = case (findPerhaps b) of
               Just (Perhaps xs, pos) -> pickUp $ map (solv . replace b pos) xs
               Nothing                -> judge b
  where
    b = ommit board


ommit :: Board -> Board
ommit = ommitBlocks . ommitCols . ommitRows


ommitRows :: Board -> Board
ommitRows = map ommitRow


ommitRow :: [Cell] -> [Cell]
ommitRow xs = ommitRow' [c | Confirmed c <- xs] xs
  where
    ommitRow' :: [Int] -> [Cell] -> [Cell]
    ommitRow' ys []     = []
    ommitRow' ys (z:zs) = case z of
                            Confirmed a -> Confirmed a : ommitRow' ys zs
                            Perhaps as  -> if length bs == 1 then
                                             Confirmed (bs !! 0) : ommitRow' ys zs
                                           else
                                             Perhaps bs : ommitRow' ys zs
                              where
                                bs = filter (\a -> not $ elem a ys) as


ommitCols :: Board -> Board
ommitCols = transpose . ommitRows . transpose


ommitBlocks :: Board -> Board
ommitBlocks = splitToBlocks . ommitRows . splitToBlocks


findPerhaps :: Board -> Maybe (Cell, (Int, Int))
findPerhaps = flip findP [0..8]


findP :: Board -> [Int] -> Maybe (Cell, (Int, Int))
findP b []     = Nothing
findP b (i:is) = case (findP' b i [0..8]) of
                   Nothing -> findP b is
                   Just x  -> Just x


findP' :: Board -> Int -> [Int] -> Maybe (Cell, (Int, Int))
findP' b i []     = Nothing
findP' b i (j:js) = case ((b !! i) !! j) of
                      Perhaps xs  -> Just (Perhaps xs, (i, j))
                      Confirmed x -> findP' b i js


pickUp :: [Maybe Board] -> Maybe Board
pickUp []     = Nothing
pickUp (x:xs) = case x of
                  Just b  -> Just b
                  Nothing -> pickUp xs


replace :: Board -> (Int, Int) -> Int -> Board
replace board (i, j) x = f ++ [replace' (board !! i) j x] ++ b
  where
    f = take i board
    b = drop (i+1) board


replace' :: [Cell] -> Int -> Int -> [Cell]
replace' row j x = f ++ [Confirmed x] ++ b
  where
    f = take j row
    b = drop (j+1) row


judge :: Board -> Maybe Board
judge b = if (and [judgeRows b, judgeCols b, judgeBlocks b]) then
            Just b
          else
             Nothing


judgeRows :: Board -> Bool
judgeRows = and . map (\r -> sumCells r == 45)


judgeCols :: Board -> Bool
judgeCols = and . map (\r -> sumCells r == 45) . transpose


judgeBlocks :: Board -> Bool
judgeBlocks = and . map (\b -> sumCells b == 45) . splitToBlocks


splitToBlocks :: Board -> [[Cell]]
splitToBlocks = concat . map (map concat) . map transpose . split3 . map split3


split3 :: [a] -> [[a]]
split3 xs = [take 3 xs, take 3 $ drop 3 xs, drop 6 xs]


sumCells :: [Cell] -> Int
sumCells cs = sum [x | Confirmed x <- cs]

--------------------------------------------------------------------------------
