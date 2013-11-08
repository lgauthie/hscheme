module Eval where

import Parser (LispVal(..), readExpr, unwordsList, ParseError)
import Data.Data (toConstr)
import Control.Monad.Error

import qualified Data.Vector as Vec
import qualified System.Environment as Sys

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; Found Values" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid Type: Expected " ++ expected
                                       ++ ", Found " ++ show found
showError (Parser parseErr) = "Parse Error At: " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (UnboundVar message var) = message ++ ": " ++ var
showError (Default message) = message

instance Show LispError where show = showError
instance Error LispError where
    noMsg  = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Complex _) = val
eval val@(Rational _) = val
eval val@(Bool _) = val
eval val@(Char _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [("+",         numericBinop (+))
    ,("-",         numericBinop (-))
    ,("*",         numericBinop (*))
    ,("/",         numericBinop div)
    ,("mod",       numericBinop mod)
    ,("quotient",  numericBinop quot)
    ,("remainder", numericBinop rem)
    ,("char?",     unaryOp Bool $ isA $ Char ' ')
    ,("bool?",     unaryOp Bool $ isA $ Bool True)
    ,("complex?",  unaryOp Bool $ isA $ Complex 0)
    ,("integer?",  unaryOp Bool $ isA $ Number 0)
    ,("list?",     unaryOp Bool $ isA $ List [])
    ,("number?",   unaryOp Bool isNumber)
    ,("pair?",     unaryOp Bool $ isA $ DottedList [Char ' '] (Char ' '))
    ,("rational?", unaryOp Bool $ isA $ Rational 0)
    ,("real?",     unaryOp Bool isReal)
    ,("string?",   unaryOp Bool $ isA $ String "")
    ,("vector?",   unaryOp Bool $ isA $ Vector Vec.empty)
    ,("symbol?",   isSymbol)
    ,("symbol->string", symbolString)
    ,("string->symbol", stringSymbol)
    ]

symbolString :: [LispVal] -> LispVal
symbolString [Atom atom] = String atom
symbolString _ = Bool False

stringSymbol :: [LispVal] -> LispVal
stringSymbol [String s] = Atom s
stringSymbol _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

isReal :: LispVal -> Bool
isReal v = any (flip isA v) [Number 0, Rational 0, Float 0]

isNumber :: LispVal -> Bool
isNumber v = any (flip isA v) [Number 0, Complex 0, Float 0, Rational 0]

isA :: LispVal -> LispVal -> Bool
isA s v = toConstr v == toConstr s

unaryOp :: (a -> LispVal) -- LispVal Type Constructor
      -> (LispVal -> a) -- The fn that will be used to evaluate the lispval
      -> [LispVal]      -- A list of LispVals to evaluate
      -> LispVal
unaryOp t op [param] = t $ op param
unaryOp _ _ _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

main :: IO ()
main = Sys.getArgs >>= print . eval . readExpr . head
