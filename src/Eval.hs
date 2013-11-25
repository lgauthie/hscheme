module Eval where

import LispData

import Parser (parse ,parseExpr)
import Control.Monad.Error
import Data.IORef (newIORef, readIORef, writeIORef)

import qualified Data.Map as M
import qualified System.Environment as S

type IOThrowsError = ErrorT LispError IO

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _   val@(String _) = return val
eval _   val@(Number _) = return val
eval _   val@(Float _) = return val
eval _   val@(Complex _) = return val
eval _   val@(Rational _) = return val
eval _   val@(Bool _) = return val
eval _   val@(Char _) = return val
eval _   val@(Vector _) = return val
eval _   (List [Atom "quote", val]) = return val
eval env (Atom name) = getVar env name
eval env (List [Atom "if", cond, conseq, alt]) =
    do result <- eval env cond
       case result of
            Bool False -> eval env alt
            Bool True  -> eval env conseq
            notBool    -> throwError $ TypeMismatch "bool" notBool
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
    makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
    makeVarargs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
    makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
    makeVarargs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
    makeVarargs varargs env [] body'
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs' env params' body' = return $ Func (map show params') varargs' body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . show

nullEnv :: IO Env
nullEnv = newIORef $ M.fromList []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . M.lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (M.lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (M.lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef (M.insert var valueRef env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    forM_ bindings $ \(key, val) -> do
        valueRef <- newIORef val
        env <- readIORef envRef
        writeIORef envRef (M.insert key valueRef env)
    return envRef

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc fn) args = liftThrows $ fn args
apply (Func p v b c) args =
    if num p /= num args && v == Nothing
       then throwError $ NumArgs (num p) args
       else (liftIO $ bindVars c $ zip p args) >>= bindVarArgs v >>= evalBody
  where
     remainingArgs = drop (length p) args
     num = toInteger . length
     evalBody env = liftM last $ mapM (eval env) b
     bindVarArgs arg env = case arg of
         Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
         Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = do
    e <- nullEnv
    forM_ primitives $ \(key, fn) -> do
        valueRef <- newIORef $ PrimitiveFunc fn
        env <- readIORef e
        writeIORef e (M.insert key valueRef env)
    return $ e

primitives :: [(String, ([LispVal] -> ThrowsError LispVal))]
primitives =
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
    ,("char?",     isChar)
    ,("bool?",     isBool)
    ,("complex?",  isComplex)
    ,("integer?",  isNumber)
    ,("list?",     isList)
    ,("number?",   isNumber)
    ,("pair?",     isDottedList)
    ,("rational?", isRational)
    ,("real?",     isReal)
    ,("string?",   isString)
    ,("vector?",   isVector)
    ,("symbol?",   isSymbol)
    ,("symbol->string", symbolString)
    ,("string->symbol", stringSymbol)
    ,("head", head')
    ,("tail", tail')
    ,("cons", cons)
    ]

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol vals = throwError $ NumArgs 1 vals

isReal :: [LispVal] -> ThrowsError LispVal
isReal [(Number _)]   = return $ Bool True
isReal [(Rational _)] = return $ Bool True
isReal [(Float _)]    = return $ Bool True
isReal  _             = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Complex _)] = return $ Bool True
isNumber  val          = isReal val

isRational :: [LispVal] -> ThrowsError LispVal
isRational [(Rational _)] = return $ Bool True
isRational  _             = return $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar [(Char _)] = return $ Bool True
isChar  _         = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString  _           = return $ Bool False

isBool :: [LispVal] -> ThrowsError LispVal
isBool [(Bool _)] = return $ Bool True
isBool  _         = return $ Bool False

isComplex :: [LispVal] -> ThrowsError LispVal
isComplex [(Complex _)] = return $ Bool True
isComplex  _            = return $ Bool False

isList :: [LispVal] -> ThrowsError LispVal
isList [(List _)] = return $ Bool True
isList  _         = return $ Bool False

isVector :: [LispVal] -> ThrowsError LispVal
isVector [(Vector _)] = return $ Bool True
isVector  _           = return $ Bool False

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList [(DottedList _ _)] = return $ Bool True
isDottedList  _                 = return $ Bool False

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
binop unpacker t op p = mapM unpacker p >>= return . t . foldl1 op

boolBinop :: (LispVal -> ThrowsError a)
          -> ([Bool] -> Bool)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop _ _ _     []  = throwError $ NumArgs 2 []
boolBinop _ _ _ val@[_] = throwError $ NumArgs 2 val
boolBinop un bfn fn p =
    mapM unpacker (tupler p) >>=
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
    args <- S.getArgs
    let prog = args !! 0
    env <- primitiveBindings
    evaled <- runIOThrows $ liftM show $ (liftThrows $ readExpr prog) >>= eval env
    putStrLn $ evaled
