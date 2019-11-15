module Main where

import System.Environment ( getArgs )
import Data.Char
import Data.List
import System.Console.GetOpt

--------------------------------------------------------------------------------

data Cell = Confirmed Int
          | Perhaps [Int]
          deriving (Show, Eq)

type Board = [[Cell]]

--------------------------------------------------------------------------------

progName = "sudoku"
version  = "v1.0.1"


main :: IO ()
main = do argv <- getArgs
          (o, n) <- parseArgs argv
          if optShowVersion o then
            putStrLn version
          else if optShowHelp o then
            putStrLn $ usageInfo header options
          else do cs <- readFile $ head n
                  let board = parseBoard cs
                  case (solv board) of
                    Just b  -> printBoard b
                    Nothing -> putStrLn "Failed to solve."

--------------------------------------------------------------------------------

-- command line options

data Options = Options { optShowVersion :: Bool
                       , optShowHelp    :: Bool
                       }

defaultOptions = Options { optShowVersion = False
                         , optShowHelp    = False
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option ['v']      ["version"]
            (NoArg (\ opts -> opts { optShowVersion = True}))
            "show version"
          , Option ['h', '?'] ["help"]
            (NoArg (\ opts -> opts { optShowHelp = True}))
            "show this message"
          ]


header :: String
header = "Usage: " ++ progName ++ " [OPTION] <input>\n\nOptions:"


parseArgs :: [String] -> IO (Options, [String])
parseArgs argv = case getOpt Permute options argv of
                   (o,n,[])   -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

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
solv board = let b = ommit board in
             case (findPerhaps b) of
               Just (Perhaps xs, pos) -> pickUp $ map (solv . replace b pos) xs
               Nothing                -> judge b


ommit :: Board -> Board
ommit = ommitBlocks . ommitCols . ommitRows


ommitRows :: Board -> Board
ommitRows = map ommitRow


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
