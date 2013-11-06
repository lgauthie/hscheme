module Eval where

import Parser (LispVal(..), readExpr)
import Data.Data (toConstr)

import qualified Data.Vector as Vec
import qualified System.Environment as Sys

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Complex _) = val
eval val@(Rational _) = val
eval val@(Bool _) = val
eval val@(Char _) = val
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
    ]

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
unpackNum (String n) =
    let parsed = reads n :: [(Integer, String)] in
    if null parsed
        then 0
        else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = Sys.getArgs >>= print . eval . readExpr . head
