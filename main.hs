module Main where

import System.Environment ( getArgs )
import Data.Char
import Data.List


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
            Nothing -> putStrLn "Failed to solve."


parseBoard :: String -> Board
parseBoard cs = map stringToCells $ lines cs


stringToCells :: String -> [Cell]
stringToCells s = map charToCell s


charToCell :: Char -> Cell
charToCell ' ' = Perhaps [1..9]
charToCell c   = Confirmed (ord c - 48)


printBoard :: Board -> IO ()
printBoard b = putStr $ unlines $ map cellsToString b


cellsToString :: [Cell] -> String
cellsToString cs = map cellToChar cs


cellToChar :: Cell -> Char
cellToChar (Confirmed x) = chr $ x + 48
cellToChar (Perhaps xs)  = '0'


solv :: Board -> Maybe Board
solv board = let b = ommit board in
             case (findPerhaps b) of
               Just (Perhaps xs, pos) -> pickUp $ map (solv . replace b pos) xs
               Nothing                -> judge b


ommit :: Board -> Board
ommit b = ommitBlocks $ ommitCols $ ommitRows b


ommitRows :: Board -> Board
ommitRows b = map ommitRow b


ommitRow :: [Cell] -> [Cell]
ommitRow xs = let cs = [c | Confirmed c <- xs] in
              ommitRow' cs xs
  where
    ommitRow' :: [Int] -> [Cell] -> [Cell]
    ommitRow' ys []     = []
    ommitRow' ys (z:zs) = case z of
                            Confirmed a -> Confirmed a : ommitRow' ys zs
                            Perhaps as  -> let bs = filter (\a -> not $ elem a ys) as in
                                           if length bs == 1
                                             then Confirmed (bs !! 0) : ommitRow' ys zs
                                             else Perhaps bs : ommitRow' ys zs


ommitCols :: Board -> Board
ommitCols b = transpose $ ommitRows $ transpose b


ommitBlocks :: Board -> Board
ommitBlocks b = splitToBlocks $ ommitRows $ splitToBlocks b


findPerhaps :: Board -> Maybe (Cell, (Int, Int))
findPerhaps b = findP b [0..8]


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
replace board (i, j) x = let f = take i board in
                         let b = drop (i+1) board in
                         f ++ [replace' (board !! i) j x] ++ b


replace' :: [Cell] -> Int -> Int -> [Cell]
replace' row j x = let f = take j row in
                   let b = drop (j+1) row in
                   f ++ [Confirmed x] ++ b


judge :: Board -> Maybe Board
judge b = if (and [judgeRows b, judgeCols b, judgeBlocks b]) then Just b else Nothing


judgeRows :: Board -> Bool
judgeRows b = and $ map (\r -> sumCells r == 45) b


judgeCols :: Board -> Bool
judgeCols b = and $ map (\r -> sumCells r == 45) $ transpose b


judgeBlocks :: Board -> Bool
judgeBlocks board = and $ map (\b -> sumCells b == 45) $ splitToBlocks board


splitToBlocks :: Board -> [[Cell]]
splitToBlocks board = concat $ map (map concat) $ map transpose $ split3 $ map split3 board


split3 :: [a] -> [[a]]
split3 xs = [take 3 xs, take 3 $ drop 3 xs, drop 6 xs]


sumCells :: [Cell] -> Int
sumCells cs = sum [x | Confirmed x <- cs]
