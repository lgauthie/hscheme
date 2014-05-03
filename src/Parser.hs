module Parser
    (parseExpr
    ,parse
    ,readExpr
    ,readExprList
    ) where

import LispData

import Data.Char (toLower, toUpper)
import Data.Complex (Complex(..))
import Data.Ratio ((%), numerator, denominator)
import Numeric (readInt, readOct, readHex)
import Control.Monad.Error (throwError)

import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)

import qualified Data.Char as C
import qualified Data.Vector as V

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

 -- Match the string 's', accepting either lowercase or uppercase chars
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = mapM caseInsensitiveChar s <?> "\"" ++ s ++ "\""

readBin :: String -> Integer
readBin s = x
  where [(x,"")] = readInt 2 (`elem` "01") C.digitToInt s

-- Symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escape :: Parser Char
escape =
    char '\\' >>
    oneOf "\"\\nrt" >>= \c ->
    return $ case c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> c

spaces :: Parser ()
spaces = skipMany1 space

-- Parsing Code
parseString :: Parser LispVal
parseString =
    char '"' >>
    many (escape <|> noneOf "\"") >>= \x ->
    char '"' >>
    return (String x)

parseChar :: Parser LispVal
parseChar =
    char '\\' >>
    (firstOrCaseInsensitiveString "space"
     <|> firstOrCaseInsensitiveString "newline"
     <|> count 1 anyChar) >>= \chars ->
    return $ case chars of
        [ch]      -> Char ch
        "space"   -> Char ' '
        "newline" -> Char '\n'
        _         -> error "Not a valid character literal"
  where
    firstOrCaseInsensitiveString [] = error "Need at least one char to match"
    firstOrCaseInsensitiveString (s:sx) =
        caseInsensitiveChar s >>= \ch ->
        optionMaybe (caseInsensitiveString sx) >>= \r ->
        return $ case r of
            Nothing -> [ch]
            Just t  -> toLower s:map toLower t

parseAtom :: Parser LispVal
parseAtom =
    (letter <|> symbol) >>= \first ->
    many (letter <|> digit <|> symbol) >>= \rest ->
    return (Atom $ first:rest)

parseBool :: Parser LispVal
parseBool =
    (char 't' <|> char 'f') >>= \b ->
    return $ Bool $ b == 't'

parseHex :: Parser LispVal
parseHex =
    char 'x' >>
    many1 hexDigit >>= \s ->
    let [(n, "")] = readHex s
    in return (Number n)

parseDec :: Parser LispVal
parseDec =
    optional (char 'd') >>
    many1 digit >>= \s ->
    return $ (Number . read) s

parseOct :: Parser LispVal
parseOct =
    char 'o' >>
    many1 octDigit >>= \s ->
    let [(n, "")] = readOct s
    in return (Number n)

parseBin :: Parser LispVal
parseBin =
    char 'b' >>
    many1 (oneOf "10") >>= \s ->
    return $ Number . readBin $ s

parseNumber :: Parser LispVal
parseNumber = parseBin
          <|> parseOct
          <|> parseHex
          <|> parseDec

parseReal :: Parser LispVal
parseReal =
    many1 digit >>= \num ->
        (oneOf "/." >>= \con ->
         many1 digit >>= \n ->
         return (con:n) <|> string "") >>= \rest ->
    return $ case rest of
        '.':_     -> Float . read $ num ++ rest
        '/':denom -> Rational $ read num % read denom
        ""        -> Number $ read num
        _         -> error "Other cases aren't numbers!"

parseBareNumber :: Parser LispVal
parseBareNumber =
    parseReal >>= \real ->
    optionMaybe (
        char '+' >>
        parseReal >>= \im ->
        char 'i' >>
        return im) >>= \c ->
    return $ case c of
        Nothing -> real
        Just im -> Complex $ toDouble real :+ toDouble im
  where
    toDouble (Float x) = x
    toDouble (Number x) = fromIntegral x
    toDouble (Rational x) = fromIntegral (numerator x) / fromIntegral (denominator x)
    toDouble _ = error "toDouble only makes sense on numeric types"

parseVector :: Parser LispVal
parseVector =
    char '[' >>
    sepBy parseExpr spaces >>= \list ->
    char ']' >>
    return (Vector $ V.fromList list)

parseLists :: Parser LispVal
parseLists =
    sepEndBy parseExpr spaces >>= \listHead ->
    optionMaybe (char '.' >> spaces >> parseExpr) >>= \listTail ->
    return $ case listTail of
        Nothing  -> List listHead
        Just val -> DottedList listHead val

parseQuote :: Parser LispVal
parseQuote =
    oneOf "\'`," >>= \c ->
    parseExpr >>= \x ->
    return $ case c of
        '\'' -> List [Atom "quote", x]
        '`'  -> List [Atom "quasiquote", x]
        ','  -> List [Atom "unquote", x]
        _    -> error "Only ['`,] are valid quote chars"

parseExpr :: Parser LispVal
parseExpr
      = parseAtom
    <|> parseQuote
    <|> parseString
    <|> parseBareNumber
    <|> parseVector
    <|> parseChar
    <|> (char '#' >> (parseNumber <|> parseBool))
    <|> (char '(' >>
         parseLists >>= \x ->
         char ')' >>
         return x)
    <|> (char ';' >>
         skipMany (noneOf "\n") >>
         char '\n' >>
         parseExpr)

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
