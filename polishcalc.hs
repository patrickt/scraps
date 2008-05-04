module Main where
  
  import Data.IORef
  import Stack
  
  readFloat :: String -> Float
  readFloat = read
  
  findCommand :: String -> (Stack -> Stack)
  findCommand cmd = case cmd of
    "+" -> add
    "-" -> sub
    "*" -> mul
    "/" -> sDiv
    "chs" -> chs
    "abs" -> sAbs
    "sqrt" -> sSqrt
    "sin" -> sSin
    "cos" -> sCos
    "tan" -> sTan
    "exp" -> sExp
    "ln" -> ln
    "ytox" -> ytox
    "rotate" -> rotate
    "clear" -> clear
    "x" -> "x"
    otherwise -> push (readFloat cmd)

  readExpr :: IORef Stack -> IO ()
  readExpr stack = do
    putStr "> "
    item <- getLine
    if (item == "exit") then return () 
      else do
        modifyIORef stack (findCommand item)
        result <- (readIORef stack) 
        print result
        readExpr stack
  
  main :: IO ()
  main = do
    theStack <- newIORef newStack
    readExpr theStack