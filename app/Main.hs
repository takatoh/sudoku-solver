module Main where

import Lib ( solv, parseBoard, printBoard )
import System.Environment ( getArgs )
import System.Console.GetOpt

--------------------------------------------------------------------------------

progName = "sudoku"
version  = "v1.1.0"


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
