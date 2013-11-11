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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Complex _) = return val
eval val@(Rational _) = return val
eval val@(Bool _) = return val
eval val@(Char _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

symbolString :: [LispVal] -> ThrowsError LispVal
symbolString [Atom atom] = return $ String atom
symbolString [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbolString vals = throwError $ NumArgs 1 vals

stringSymbol :: [LispVal] -> ThrowsError LispVal
stringSymbol [String s] = return $ Atom s
stringSymbol [notStr] = throwError $ TypeMismatch "string" notStr
stringSymbol vals = throwError $ NumArgs 1 vals

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol vals = throwError $ NumArgs 1 vals

isReal :: LispVal -> Bool
isReal v = any (flip isA v) [Number 0, Rational 0, Float 0]

isNumber :: LispVal -> Bool
isNumber v = any (flip isA v) [Number 0, Complex 0, Float 0, Rational 0]

isA :: LispVal -> LispVal -> Bool
isA s v = toConstr v == toConstr s

unaryOp :: (a -> LispVal) -- LispVal Type Constructor
        -> (LispVal -> a) -- The fn that will be used to evaluate the lispval
        -> [LispVal]      -- A list of LispVals to evaluate
        -> ThrowsError LispVal
unaryOp t op [param] = return $ t $ op param
unaryOp _ _ vals = throwError $ NumArgs 1 vals

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinop _     []  = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n in
    if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

main :: IO ()
main = Sys.getArgs >>= print . eval . readExpr . head
