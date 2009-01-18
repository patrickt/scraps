-- This is a parser for the HQ9+ language.
-- For more information, see http://www.cliff.biffle.org/esoterica/hq9plus.html
-- This is a purely-functional, Haskell-based parser.

module Main where
  
  import System.Environment -- for command-line arguments
  import Text.Printf        -- for simplified printing
  
  pluralize :: Integer -> String
  pluralize 1 = "bottle"
  pluralize _ = "bottles"
  
  -- prints out 99 bottles of beer on the wall. recursive and functional.
  bottles :: Integer -> IO ()
  bottles 0 = putStrLn "NO MOAR BEER."
  bottles n =
    let b = (pluralize n) in
      printf "%d %s of beer on the wall,\n" n b          >>
      printf "%d %s of beer,\n" n b                      >>
      printf "Take one down, pass it around,\n"          >>
      printf "%d %s of beer on the wall\n\n" (n - 1) b   >>
      bottles (n - 1)
  
  -- Prints out all passed arguments.
  printquine :: IO ()
  printquine = getArgs >>= (putStrLn . unwords)
  
  -- Increments the accumulator. You can insert print statements to verify correctness.
  increment :: Char -> Integer -> Integer
  increment '+' accum = accum + 1
  increment _ accum = accum
  
  -- The actual parsing function.
  
   
  parse :: Integer -> String -> IO ()
  parse _ "" = return ()
  parse accum string = do
    let c = (string !! 0) in do
      case c of
        'h' -> putStrLn "Hello, world."
        'q' -> printquine
        '9' -> bottles 9
        '+' -> return ()
      -- print accum -- Uncomment this to prove that the accumulator working.
      parse (increment c accum) (drop 1 string)
  
  main :: IO ()
  main = do
    args <- getArgs
    parse 1 (unwords args)
  -- Alternatively, you can do this (more concise but less readable)
  -- main = getArgs >>= (parse 1 . unwords)
