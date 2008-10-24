module Main where
  import Monad
  import Data.Char ( toUpper, chr, ord )
  import Numeric
  import System.Environment
  import Text.ParserCombinators.Parsec hiding (spaces)
  
  -- An algebraic data type.
  data LispVal = Atom String 
               | List [LispVal] 
               | DottedList [LispVal] LispVal
               | Number Integer
               | String [Char]
               | Character Char
               | Boolean Bool
               
               
  -- =========================================================
  -- = More parser combinators which Parsec doesn't include. =
  -- =========================================================

  -- |Case-insensitive variant of Parsec's 'char' function.
  caseChar        :: Char -> CharParser st Char
  caseChar c       = satisfy (\x -> toUpper x == toUpper c)

  -- |Case-insensitive variant of Parsec's 'string' function.
  caseString      :: String -> CharParser st [Char]
  caseString cs    = mapM caseChar cs <?> cs
  
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
  
  -- Characters
  parseCharacter :: Parser LispVal
  parseCharacter = do
    try $ string "#\\"
    value <- try (caseString "newline" <|> caseString "space") <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
      "space" -> ' '
      "newline" -> '\n'
      otherwise -> (value !! 0)
  
  -- Atoms
  parseAtom :: Parser LispVal
  parseAtom = do 
    let symbol = oneOf "!$&|*+-/:<=>?@^_~"
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
    try $ string "#x"
    hexDigits <- many1 hexDigit
    let converted = fst $ ((readHex hexDigits) !! 0)
    return $ Number converted
  
  parseOct :: Parser LispVal
  parseOct = do
    try $ string "#o"
    octDigits <- many1 octDigit
    let converted = fst $ ((readOct octDigits) !! 0)
    return $ Number converted
    
  parseAllLists :: Parser LispVal
  parseAllLists = do
    char '('
    x <- (try parseList) <|> parseDottedList
    char ')'
    return x
  
  parseList :: Parser LispVal
  parseList = List `liftM` (sepBy parseExpr spaces)
  
  parseDottedList :: Parser LispVal
  parseDottedList = do 
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
  
  parseQuoted :: Parser LispVal
  parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
      
  -- Putting it all together
  parseExpr :: Parser LispVal
  parseExpr = parseAtom <|> parseCharacter <|> parseString <|> parseNumber <|> parseBool <|> parseQuoted <|> parseAllLists

  readExpr :: String -> String
  readExpr input = case parse parseExpr "Scheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value."
    
  spaces :: Parser ()
  spaces = skipMany1 space

  main :: IO ()
  main = do 
    args <- getArgs
    putStrLn $ readExpr $ args !! 0