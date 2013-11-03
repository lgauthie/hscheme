module Main where

--import Control.Monad (liftM)
import Numeric (readInt, readOct, readHex)
import Data.Complex
import Data.Ratio
import Data.Array
import Data.Char (toLower, toUpper)

import Text.Parsec.String (Parser)
import Text.Parsec hiding (spaces)

import qualified System.Environment as Sys
import qualified Data.Char as Char

-- Data
data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Integer LispVal)
    | Number Integer
    | Float Double
    | Complex (Complex Double)
    | Rational (Ratio Integer)
    | String String
    | Char Char
    | Bool Bool
    deriving (Show)

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

 -- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = mapM caseInsensitiveChar s <?> "\"" ++ s ++ "\""

readBin :: String -> Integer
readBin s = x
  where [(x,"")] = readInt 2 (`elem` "01") Char.digitToInt s

-- Symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escape :: Parser Char
escape = do
    char '\\'
    c <- oneOf "\"\\nrt"
    return $ case c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> c

spaces :: Parser ()
spaces = skipMany1 space

-- Parsing Code
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escape <|> noneOf "\""
    char '"'
    return $ String x

parseChar :: Parser LispVal
parseChar = do
    char '\\'
    chars <- firstOrCaseInsensitiveString "space"
         <|> firstOrCaseInsensitiveString "newline"
         <|> count 1 anyChar
    return $ case chars of
        [ch] -> Char ch
        "space" -> Char ' '
        "newline" -> Char '\n'
  where
    firstOrCaseInsensitiveString (s:sx) = do
        ch <- caseInsensitiveChar s
        r <- optionMaybe $ caseInsensitiveString sx
        return $ case r of
            Nothing -> [ch]
            Just t  -> [toLower s] ++ map toLower t

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    b <- char 't' <|> char 'f'
    return $ case b of
        't' -> Bool True
        'f' -> Bool False

parseHex :: Parser LispVal
parseHex = do
    char 'x'
    s <- many1 $ hexDigit
    let [(n, "")] = readHex s
    return $ Number n

parseDec :: Parser LispVal
parseDec = do
    optional $ char 'd'
    s <- many1 $ digit
    return $ (Number . read) s

parseOct :: Parser LispVal
parseOct = do
    char 'o'
    s <- many1 $ octDigit
    let [(n, "")] = readOct s
    return $ Number n

parseBin :: Parser LispVal
parseBin = do
    char 'b'
    s <- many1 $ oneOf "10"
    return $ (Number . readBin) s

parseNumber :: Parser LispVal
parseNumber = parseBin
          <|> parseOct
          <|> parseHex
          <|> parseDec

parseReal :: Parser LispVal
parseReal = do
    num <- many1 $ digit
    rest <- do
        con <- oneOf "/."
        n <- many1 $ digit
        return $ [con] ++ n
        <|> string ""
    return $ case rest of
        '.':_     -> (Float . read) (num ++ rest)
        '/':denom -> Rational (read num % read denom)
        ""        -> Number $ read num

parseBareNumber :: Parser LispVal
parseBareNumber = do
    real <- parseReal
    c <- optionMaybe $ do
        char '+'
        im <- parseReal
        char 'i'
        return im
    return $ case c of
        Nothing -> real
        Just im -> Complex (toDouble(real) :+ toDouble(im))
  where
    toDouble (Float x) = x
    toDouble (Number x) = fromIntegral x
    toDouble (Rational x) = (fromIntegral (numerator x))/(fromIntegral (denominator x))

parseVector :: Parser LispVal
parseVector = do
    char '('
    list <- sepBy parseExpr spaces
    char ')'
    return $ Vector $ listArray (1,len(list)) list
  where
    len x = toInteger $ length x

parseLists :: Parser LispVal
parseLists = do
    listHead <- sepEndBy parseExpr spaces
    listTail <- optionMaybe $ char '.' >> spaces >> parseExpr
    return $ case listTail of
        Nothing  -> List listHead
        Just val -> DottedList listHead val

parseQuote :: Parser LispVal
parseQuote = do
    c <- oneOf "\'`,"
    x <- parseExpr
    return $ case c of
        '\'' -> List [Atom "quote", x]
        '`'  -> List [Atom "quasiquote", x]
        ','  -> List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseQuote
    <|> parseString
    <|> parseBareNumber
    <|> do char '#'
           parseNumber <|> parseChar <|> parseBool <|> parseVector
    <|> do char '('
           x <- parseLists
           char ')'
           return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> "Value Found: " ++ show val

main :: IO ()
main = do
    args <- Sys.getArgs
    putStrLn $ readExpr $ args !! 0
