module Main where

--import Control.Monad (liftM)
import Numeric (readInt, readOct, readHex)
import Data.Complex
import Data.Ratio
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
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

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
    string "#\\"
    chars <- (try . caseInsensitiveString) "space"
         <|> (try . caseInsensitiveString) "newline"
         <|> count 1 anyChar
    return $ case (map toLower chars) of
        [_] -> Char (head chars)
        "space" -> Char ' '
        "newline" -> Char '\n'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    b <- (try . string) "#t" <|> (try . string) "#f"
    return $ case b of
        "#t" -> Bool True
        "#f" -> Bool False

parseHex :: Parser LispVal
parseHex = do
    (try . string) "#x"
    s <- many1 $ hexDigit
    let [(n, "")] = readHex s
    return $ Number n

parseDec :: Parser LispVal
parseDec = do
    optional $ (try . string) "#d"
    s <- many1 $ digit
    return $ (Number . read) s

parseOct :: Parser LispVal
parseOct = do
    (try . string) "#o"
    s <- many1 $ octDigit
    let [(n, "")] = readOct s
    return $ Number n

parseBin :: Parser LispVal
parseBin = do
    (try . string) "#b"
    s <- many1 $ oneOf "10"
    return $ (Number . readBin) s

parseNumber :: Parser LispVal
parseNumber = parseBin <|> parseOct <|> parseHex <|> parseDec

parseFloat :: Parser LispVal
parseFloat = do
    whole <- many1 $ digit
    char '.'
    dec <- many1 $ digit
    return $ (Float . read) (whole++"."++dec)

parseComplex :: Parser LispVal
parseComplex = do
    real <- try parseFloat <|> parseDec
    char '+'
    im <- try parseFloat <|> parseDec
    char 'i'
    return $ Complex (toDouble(real) :+ toDouble(im))
  where
    toDouble (Float x) = x
    toDouble (Number x) = fromIntegral x

parseRational :: Parser LispVal
parseRational = do
    numer <- parseDec
    char '/'
    denom <- parseDec
    return $ Rational (toNum(numer) % toNum(denom))
  where
    toNum (Number x) = x

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> try parseString
    <|> try parseChar
    <|> try parseRational
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> "Value Found: " ++ show val

main :: IO ()
main = do
    args <- Sys.getArgs
    putStrLn $ readExpr $ args !! 0
