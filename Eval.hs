module Eval where

import Parser (LispVal(..), readExpr)
import Data.Data (toConstr)

import qualified System.Environment as Sys

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [("+", numericBinop (+))
    ,("-", numericBinop (-))
    ,("*", numericBinop (*))
    ,("/", numericBinop div)
    ,("mod", numericBinop mod)
    ,("quotient", numericBinop quot)
    ,("remainder", numericBinop rem)
    ,("char?",     unaryOp Bool $ isA "Char")
    ,("bool?",     unaryOp Bool $ isA "Bool")
    ,("complex?",  unaryOp Bool $ isA "Complex")
    ,("integer?",  unaryOp Bool $ isA "Number")
    ,("list?",     unaryOp Bool $ isA "List")
    ,("number?",   unaryOp Bool isNumber)
    ,("pair?",     unaryOp Bool $ isA "DottedList")
    ,("rational?", unaryOp Bool $ isA "Rational")
    ,("real?",     unaryOp Bool isReal)
    ,("string?",   unaryOp Bool $ isA "String")
    ,("vector?",   unaryOp Bool $ isA "Vector")
    ]

isReal :: LispVal -> Bool
isReal v = any (flip isA v) ["Number", "Rational", "Float"]

isNumber :: LispVal -> Bool
isNumber v = any (flip isA v) ["Number", "Rational", "Float", "Complex"]

isA :: String -> LispVal -> Bool
isA s v = (show . toConstr $ v) == s

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
unpackNum (String n) =
    let parsed = reads n :: [(Integer, String)] in
    if null parsed
        then 0
        else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = Sys.getArgs >>= print . eval . readExpr . head
