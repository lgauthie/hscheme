module Eval where

import Parser (LispVal(..), ParseError, parse, parseExpr, unwordsList)
import Data.Data (toConstr)
import Data.Map (Map)
import Control.Monad.Error

import qualified Data.Vector as Vec
import qualified Data.Map as Map
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
                                  ++ " args; Found Values " ++ unwordsList found
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
eval (List [Atom "if", cond, conseq, alt]) =
    do result <- eval cond
       case result of
            Bool False -> eval alt
            Bool True  -> eval conseq
            notBool    -> throwError $ TypeMismatch "bool" notBool
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe err ($ args) $ Map.lookup func primitives
  where
    err = throwError $ NotFunction "Unrecognized primitive function" func

primitives :: Map String ([LispVal] -> ThrowsError LispVal)
primitives = Map.fromList
    [("+",         binop unpackNum Number (+))
    ,("-",         binop unpackNum Number (-))
    ,("*",         binop unpackNum Number (*))
    ,("/",         binop unpackNum Number div)
    ,("mod",       binop unpackNum Number mod)
    ,("quotient",  binop unpackNum Number quot)
    ,("remainder", binop unpackNum Number rem)
    ,("=",         boolBinop unpackNum and (==))
    ,("<",         boolBinop unpackNum and (<))
    ,(">",         boolBinop unpackNum and (>))
    ,("/=",        boolBinop unpackNum and (/=))
    ,(">=",        boolBinop unpackNum and (>=))
    ,("<=",        boolBinop unpackNum and (<=))
    ,("&&",        boolBinop unpackBool and (&&))
    ,("||",        boolBinop unpackBool or (||))
    ,("string=?",  boolBinop unpackStr and (==))
    ,("string<?",  boolBinop unpackStr and (<))
    ,("string>?",  boolBinop unpackStr and (>))
    ,("string<=?", boolBinop unpackStr and (<=))
    ,("string>=?", boolBinop unpackStr and (>=))
    ,("char?",     unop Bool $ is $ Char ' ')
    ,("bool?",     unop Bool $ is $ Bool True)
    ,("complex?",  unop Bool $ is $ Complex 0)
    ,("integer?",  unop Bool $ is $ Number 0)
    ,("list?",     unop Bool $ is $ List [])
    ,("number?",   unop Bool isNumber)
    ,("pair?",     unop Bool $ is $ DottedList [Char ' '] (Char ' '))
    ,("rational?", unop Bool $ is $ Rational 0)
    ,("real?",     unop Bool isReal)
    ,("string?",   unop Bool $ is $ String "")
    ,("vector?",   unop Bool $ is $ Vector Vec.empty)
    ,("symbol?",   isSymbol)
    ,("symbol->string", symbolString)
    ,("string->symbol", stringSymbol)
    ,("head", head')
    ,("tail", tail')
    ,("cons", cons)
    ]

head' :: [LispVal] -> ThrowsError LispVal
head' [List (x:_)] = return x
head' [DottedList (x:_) _] = return x
head' [badArg] = throwError $ TypeMismatch "pair" badArg
head' badArgList = throwError $ NumArgs 1 badArgList

tail' :: [LispVal] -> ThrowsError LispVal
tail' [List (_:xs)] = return $ List xs
tail' [DottedList [_] x] = return x
tail' [DottedList (_:xs) x] = return $ DottedList xs x
tail' [badArg] = throwError $ TypeMismatch "pair" badArg
tail' badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n in
    if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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
isReal v = any (flip is v) [Number 0, Rational 0, Float 0]

isNumber :: LispVal -> Bool
isNumber v = any (flip is v) [Number 0, Complex 0, Float 0, Rational 0]

is :: LispVal -> LispVal -> Bool
is s v = toConstr v == toConstr s

unop :: (a -> LispVal) -- LispVal Type Constructor
     -> (LispVal -> a) -- The fn that will be used to evaluate the lispval
     -> [LispVal]      -- A list of LispVals to evaluate
     -> ThrowsError LispVal
unop t op [param] = return $ t $ op param
unop _ _ vals = throwError $ NumArgs 1 vals

binop :: (LispVal -> ThrowsError a) -- Unpacker
      -> (a -> LispVal)             -- LispVal Type Constructor
      -> (a -> a -> a)
      -> [LispVal]
      -> ThrowsError LispVal
binop _ _ _     []  = throwError $ NumArgs 2 []
binop _ _ _ val@[_] = throwError $ NumArgs 2 val
binop unpacker t op params = mapM unpacker params >>= return . t . foldl1 op

boolBinop :: (LispVal -> ThrowsError a)
          -> ([Bool] -> Bool)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop _ _ _     []  = throwError $ NumArgs 2 []
boolBinop _ _ _ val@[_] = throwError $ NumArgs 2 val
boolBinop un bfn fn params = mapM unpacker (tupler params) >>=
                         return . Bool . bfn . map (\(x,y) -> fn x y)
  where
    tupler [] = error "Can't Tuple empty list"
    tupler [_] = []
    tupler (x:y:ys) = (x, y):tupler(y:ys)
    unpacker (x, y) = do
        x1 <- un x
        y1 <- un y
        return (x1, y1)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "Can only extract Right val"

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

main :: IO ()
main = do
    args <- Sys.getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
