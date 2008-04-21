-- This is a parser for the HQ9+ language.
-- For more information, see http://www.cliff.biffle.org/esoterica/hq9plus.html
-- This is a purely-functional, Haskell-based parser.

module Main where
  
  import System.Environment -- for command-line arguments
  import Data.IORef         -- for IORefs
  
  -- prints out 99 bottles of beer on the wall. recursive and functional.
  bottles :: Integer -> IO ()
  
  bottles 0 = putStrLn "NO MOAR BEER." -- a tragic situation
  
  bottles n = do
    putStrLn $ (show n) ++ " bottles of beer on the wall,"
    putStrLn $ (show n) ++ " bottles of beer,"
    putStrLn $ "Take one down, pass it around,"
    putStrLn $ (show (n - 1)) ++ " bottles of beer on the wall.\n"
    bottles (n - 1)
  
  
  -- Increments the accumulator. You can insert print statements to verify correctness.
  increment :: IORef Integer -> IO ()
  
  increment ref = modifyIORef ref (\x -> x + 1)
  
  parse :: IORef Integer -> Char -> IO ()
  parse ref c =
    case c of
      'h' -> putStrLn "Hello, world."
      'q' -> putStrLn "q"
      '9' -> bottles 9
      '+' -> increment ref
      otherwise -> return ()
  
  main :: IO ()
  main = do
    ref <- newIORef 0 -- initialize the accumulator to 0
    a <- getArgs
    let firstArg = a !! 0
    mapM (parse ref) firstArg -- monadically map the parse command onto the arguments
    return ()
