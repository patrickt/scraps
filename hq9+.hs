module Main where
  
  import System.Environment -- for command-line arguments
  import Data.IORef         -- for IORefs
  
  -- prints out 99 bottles of beer on the wall. recursive and functional.
  bottles :: Integer -> IO ()
  
  bottles 0 = do
    putStrLn "NO MOAR BEER." -- a tragic situation
    return ()                -- box up an empty IO monad
  
  bottles n = do
    putStrLn $ (show n) ++ " bottles of beer on the wall,"
    putStrLn $ (show n) ++ " bottles of beer,"
    putStrLn $ "Take one down, pass it around,"
    putStrLn $ (show (n - 1)) ++ " bottles of beer on the wall.\n"
    bottles (n - 1)
  
  
  increment :: IORef Integer -> IO ()
  increment ref = do
    -- Uncomment this if you want proof that the accumulator is working
    -- value <- readIORef ref -- read an IO monad and unbox it into a value
    -- putStrLn ("Accumulating: " ++ (show value))
    modifyIORef ref (\x -> x + 1)
    return () -- box up an empty IO monad.
  
  parse :: IORef Integer -> Char -> IO ()
  parse ref c =
    case c of
      'h' -> putStrLn "Hello, world."
      'q' -> putChar 'q'
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
