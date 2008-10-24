module Main where
  import Monad
  import Numeric
  import System.Environment
  import Text.ParserCombinators.Parsec hiding (spaces)
  
  -- An algebraic data type.
  data LispVal = Atom String 
               | List [LispVal] 
               | DottedList [LispVal] LispVal
               | Number Integer
               | String [Char]
               | Boolean Bool
               
  -- =====================================
  -- = Parsers for the above data types. =
  -- =====================================
    
  -- Strings
  escapedChars :: Parser String
  escapedChars = do
    char '\\' -- backslash
    x <- oneOf "\\\"nrt" -- backslash or "
    return $ case x of
      't' -> "\t"
      'n' -> "\n"
      'r' -> "\r"
      otherwise -> [x]
      
  parseString :: Parser LispVal
  parseString = do 
    char '"'
    chrs <- many $ many1 (noneOf "\"\\") <|> escapedChars
    char '"'
    return $ String $ concat chrs
  
  -- Booleans
  parseBool :: Parser LispVal
  parseBool = do
    char '#'
    value <- oneOf "tf"
    return $ case value of
      't' -> Boolean True
      'f' -> Boolean False
    
  -- Atoms
  parseAtom :: Parser LispVal
  parseAtom = do 
    let symbol = oneOf "!$#&|*+-/:<=>?@^_~"
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first : rest
  
  -- Numbers
  parseNumber :: Parser LispVal
  parseNumber = parseDec <|> parseHex <|> parseOct
      
  parseDec :: Parser LispVal
  parseDec = many1 digit >>= (return . Number . read)
  
  parseHex :: Parser LispVal
  parseHex = do
    string "#x"
    hexDigits <- many1 hexDigit
    let converted = fst $ ((readHex hexDigits) !! 0)
    return $ Number converted
  
  parseOct :: Parser LispVal
  parseOct = do
    string "#o"
    octDigits <- many1 octDigit
    let converted = fst $ ((readOct octDigits) !! 0)
    return $ Number converted
  
      
  -- Putting it all together
  parseExpr :: Parser LispVal
  parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool

  readExpr :: String -> String
  readExpr input = case parse parseExpr "Scheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
    
  spaces :: Parser ()
  spaces = skipMany1 space

  main :: IO ()
  main = do 
    args <- getArgs
    putStrLn $ readExpr $ args !! 0