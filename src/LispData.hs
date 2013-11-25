module LispData
    (LispVal(..)
    ,LispError(..)
    ,ThrowsError
    ,VarMap
    ,Env
    ) where

import Control.Monad.Error (Error, noMsg, strMsg)
import Data.Complex (Complex, realPart, imagPart)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Ratio (Ratio, numerator, denominator)
import Data.Vector (Vector)
import Text.Parsec (ParseError)

import qualified Data.Vector as V

type VarMap = Map String (IORef LispVal)
type Env = IORef VarMap

type ThrowsError = Either LispError

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

data LispVal
    = Atom String
    | Bool Bool
    | Char Char
    | Complex (Complex Double)
    | DottedList [LispVal] LispVal
    | Float Double
    | List [LispVal]
    | Number Integer
    | Rational (Ratio Integer)
    | String String
    | Vector (Vector LispVal)
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func {params :: [String]
           ,vararg :: (Maybe String)
           ,body :: [LispVal]
           ,closure :: Env
           }

showVal :: LispVal -> String
showVal val = case val of
    (Atom name)       -> name
    (Bool False)      -> "#f"
    (Bool True)       -> "#t"
    (Char c)          -> "#\\" ++ [c]
    (Complex c)       -> show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
    (DottedList h t)  -> "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
    (Float f)         -> show f
    (List contents)   -> "(" ++ unwordsList contents ++ ")"
    (Number num)      -> show num
    (Rational r)      -> show (numerator r) ++ "/" ++ show (denominator r)
    (String contents) -> "\"" ++ contents ++ "\""
    (Vector contents) -> "[" ++ unwordsList (V.toList contents) ++ "]"
    (PrimitiveFunc _) -> "<primitive>"
    (Func {params = args, vararg = varargs, body = _, closure = _}) ->
      "(lambda (" ++ unwords (map show args) ++
         (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal
