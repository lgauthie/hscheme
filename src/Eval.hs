module Eval where

import LispData

import Parser (readExprList, readExpr)
import Data.Complex (Complex)
import Data.Ratio (Ratio)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.Error (MonadError, throwError, catchError, runErrorT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, liftM)

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified System.Environment as S
import qualified System.IO as IO
import qualified Control.Monad.ST as ST

eval :: Env      -- The current envirionment
     -> LispVal  -- The LispVal to be evaluated
     -> IOThrowsError LispVal

-- evals for language primitives just return the value
eval _   val@(String _) = return val
eval _   val@(Number _) = return val
eval _   val@(Float _) = return val
eval _   val@(Complex _) = return val
eval _   val@(Rational _) = return val
eval _   val@(Bool _) = return val
eval _   val@(Char _) = return val
eval _   val@(Vector _) = return val
eval _   val@(DottedList _ _) = return val
eval _   val@(PrimitiveFunc _) = return val
eval _   val@(Func _ _ _ _) = return val
eval _   val@(List []) = return val
eval _   val@(IOFunc _) = return . String $ show val
eval _   val@(Port _) = return . String $ show val

-- Eval quotes
eval _   (List [Atom "quote", val]) = return val

-- Eval a varaible
eval env (Atom name) = getVar env name

-- Conditionals -- currently only if is implemented
eval env (List [Atom "if", cond, conseq, alt]) =
    do result <- eval env cond
       case result of
            Bool False -> eval env alt
            Bool True  -> eval env conseq
            notBool    -> throwError $ TypeMismatch "bool" notBool

-- Load a file into the current environment
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)

-- Set/Define varaibles
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

-- Define functions (works lists and dotted lists)
eval env (List (Atom "define":List (Atom var:params'):body')) =
    makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params') varargs:body')) =
    makeVarargs varargs env params' body' >>= defineVar env var

-- Make anonymous functions (works lists and dotted lists)
eval env (List (Atom "lambda":List params':body')) =
    makeNormalFunc env params' body'
eval env (List (Atom "lambda":DottedList params' varargs:body')) =
    makeVarargs varargs env params' body'
eval env (List (Atom "lambda":varargs@(Atom _):body')) =
    makeVarargs varargs env [] body'

-- Apply a function to a list of values
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

-- Creates a blank envirionment with no bound functions or varaibles
nullEnv :: IO Env
nullEnv = newIORef $ M.fromList []

-- Used to generate the default environment with
-- Primitive builtin functions, and IO functions
primitiveBindings :: IO Env
primitiveBindings = do
    e <- nullEnv
    forM_ primitives $ \(key, fn) -> do
        valueRef <- newIORef $ PrimitiveFunc fn
        env <- readIORef e
        writeIORef e (M.insert key valueRef env)
    forM_ ioPrimitives $ \(key, fn) -> do
        valueRef <- newIORef $ IOFunc fn
        env <- readIORef e
        writeIORef e (M.insert key valueRef env)
    return $ e

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- Check if a name is bound for the current environment
isBound :: Env    -- The current environment
        -> String -- The varaible to check if bound
        -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . M.lookup var

-- Get the value of a bound varaible
getVar :: Env     -- The current environment
       -> String  -- The variable to get the value from
       -> IOThrowsError LispVal
getVar envRef var  =  do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (M.lookup var env)

-- Set the value of a bound variable
setVar :: Env      -- The current environment
       -> String   -- Name of the variable to be set
       -> LispVal  -- The value that variable will be set to
       -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (M.lookup var env)
    return value

-- Define a single variable
defineVar :: Env      -- The current environment
          -> String   -- Name of the variable to be defined
          -> LispVal  -- The value that varaible will be set to
          -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef (M.insert var valueRef env)
          return value

-- Bind a bunch of variables at the same time
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    forM_ bindings $ \(key, val) -> do
        valueRef <- newIORef val
        env <- readIORef envRef
        writeIORef envRef (M.insert key valueRef env)
    return envRef

apply :: LispVal   -- Lisp function to be applied
      -> [LispVal] -- Values to apply the function to
      -> IOThrowsError LispVal
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
apply (IOFunc func) args = func args
apply val _ = throwError $ NotFunction "Trying to apply non function value" $ show val

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
    [("apply",             applyProc)
    ,("open-input-file",   makePort IO.ReadMode)
    ,("open-output-file",  makePort IO.WriteMode)
    ,("close-port",        closePort)
    ,("read",              readProc)
    ,("write",             writeProc)
    ,("read-contents",     readContents)
    ,("read-all",          readAll)
    ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc [] =  throwError $ Default "Trying to apply function to no arguments"

makePort :: IO.IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ IO.openFile filename mode
makePort _ [val] = throwError $ TypeMismatch "string" val
makePort _ vals = throwError $ NumArgs 1 vals

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ IO.hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port IO.stdin]
readProc [Port port] = (liftIO $ IO.hGetLine port) >>= liftThrows . readExpr
readProc [val] = throwError $ TypeMismatch "port" val
readProc vals = throwError $ NumArgs 1 vals

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port IO.stdout]
writeProc [obj, Port port] = liftIO $ IO.hPrint port obj >> (return $ Bool True)
writeProc _ = throwError $ Default "Error writing"

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents vals = throwError $ NumArgs 1 vals

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [val] = throwError $ TypeMismatch "string" val
readAll vals = throwError $ NumArgs 1 vals

primitives :: [(String, ([LispVal] -> ThrowsError LispVal))]
primitives =
    [("+",         plus)
    ,("-",         minus)
    ,("*",         mul)
    ,("/",         div')
    ,("mod",       binop unpackNum Number mod)
    ,("quotient",  binop unpackNum Number quot)
    ,("remainder", binop unpackNum Number rem)
    ,("=",         numEq)
    ,("<",         numLt)
    ,(">",         numGt)
    ,("/=",        numNe)
    ,(">=",        numGe)
    ,("<=",        numLe)
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
    ,("vector-set!", vecSet)
    ,("vector-ref",  vecRef)
    ,("vector-len",  vecLen)
    ,("head", head')
    ,("tail", tail')
    ,("cons", cons)
    ]

plus :: [LispVal] -> ThrowsError LispVal
plus p@((Number _):_) = binop unpackNum Number (+) p
plus p@((Float _):_) = binop unpackFloat Float (+) p
plus p@((Complex _):_) = binop unpackComplex Complex (+) p
plus p@((Rational _):_) = binop unpackRational Rational (+) p
plus (notNum:_) = throwError $ TypeMismatch "numeric" notNum
plus [] = throwError $ NumArgs 2 []

minus :: [LispVal] -> ThrowsError LispVal
minus p@((Number _):_) = binop unpackNum Number (-) p
minus p@((Float _):_) = binop unpackFloat Float (-) p
minus p@((Complex _):_) = binop unpackComplex Complex (-) p
minus p@((Rational _):_) = binop unpackRational Rational (-) p
minus (notNum:_) = throwError $ TypeMismatch "numeric" notNum
minus [] = throwError $ NumArgs 2 []

mul :: [LispVal] -> ThrowsError LispVal
mul p@((Number _):_) = binop unpackNum Number (*) p
mul p@((Float _):_) = binop unpackFloat Float (*) p
mul p@((Complex _):_) = binop unpackComplex Complex (*) p
mul p@((Rational _):_) = binop unpackRational Rational (*) p
mul (notNum:_) = throwError $ TypeMismatch "numeric" notNum
mul [] = throwError $ NumArgs 2 []

div' :: [LispVal] -> ThrowsError LispVal
div' p@((Number _):_) = binop unpackNum Number div p
div' p@((Float _):_) = binop unpackFloat Float (/) p
div' p@((Complex _):_) = binop unpackComplex Complex (/) p
div' p@((Rational _):_) = binop unpackRational Rational (/) p
div' (notNum:_) = throwError $ TypeMismatch "numeric" notNum
div' [] = throwError $ NumArgs 2 []

numEq :: [LispVal] -> ThrowsError LispVal
numEq p@((Number _):_) = boolBinop unpackNum and (==) p
numEq p@((Float _):_) = boolBinop unpackFloat and (==) p
numEq p@((Rational _):_) = boolBinop unpackRational and (==) p
numEq (notNum:_) = throwError $ TypeMismatch "numeric" notNum
numEq [] = throwError $ NumArgs 2 []

numNe :: [LispVal] -> ThrowsError LispVal
numNe p@((Number _):_) = boolBinop unpackNum and (/=) p
numNe p@((Float _):_) = boolBinop unpackFloat and (/=) p
numNe p@((Rational _):_) = boolBinop unpackRational and (/=) p
numNe (notNum:_) = throwError $ TypeMismatch "numeric" notNum
numNe [] = throwError $ NumArgs 2 []

numGt :: [LispVal] -> ThrowsError LispVal
numGt p@((Number _):_) = boolBinop unpackNum and (>) p
numGt p@((Float _):_) = boolBinop unpackFloat and (>) p
numGt p@((Rational _):_) = boolBinop unpackRational and (>) p
numGt (notNum:_) = throwError $ TypeMismatch "numeric" notNum
numGt [] = throwError $ NumArgs 2 []

numGe :: [LispVal] -> ThrowsError LispVal
numGe p@((Number _):_) = boolBinop unpackNum and (>=) p
numGe p@((Float _):_) = boolBinop unpackFloat and (>=) p
numGe p@((Rational _):_) = boolBinop unpackRational and (>=) p
numGe (notNum:_) = throwError $ TypeMismatch "numeric" notNum
numGe [] = throwError $ NumArgs 2 []

numLt :: [LispVal] -> ThrowsError LispVal
numLt p@((Number _):_) = boolBinop unpackNum and (<) p
numLt p@((Float _):_) = boolBinop unpackFloat and (<) p
numLt p@((Rational _):_) = boolBinop unpackRational and (<) p
numLt (notNum:_) = throwError $ TypeMismatch "numeric" notNum
numLt [] = throwError $ NumArgs 2 []

numLe :: [LispVal] -> ThrowsError LispVal
numLe p@((Number _):_) = boolBinop unpackNum and (<=) p
numLe p@((Float _):_) = boolBinop unpackFloat and (<=) p
numLe p@((Rational _):_) = boolBinop unpackRational and (<=) p
numLe (notNum:_) = throwError $ TypeMismatch "numeric" notNum
numLe [] = throwError $ NumArgs 2 []

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

-- Set the value of index n to a new value
vecSet :: [LispVal] -> ThrowsError LispVal
vecSet ((Vector v):(Number n):[val])
    | V.length v < index = return $ ST.runST $ do
        vec <- V.thaw v
        VM.write vec index val
        V.freeze vec >>= return . Vector
    | otherwise = throwError $ Index "Index out of bounds" n
  where index = fromIntegral n
vecSet vals = throwError $ NumArgs 3 vals

-- Get the value at index n of a Lisp Vector
vecRef :: [LispVal] -> ThrowsError LispVal
vecRef ((Vector v):[(Number n)]) = maybe err return (v V.!? index)
  where
    index = fromIntegral n :: Int
    err = throwError $ Index "Index out of bounds" n
vecRef vals = throwError $ NumArgs 2 vals

vecLen :: [LispVal] -> ThrowsError LispVal
vecLen [(Vector v)] = return $ Number len
  where len = toInteger $ V.length v
vecLen vals = throwError $ NumArgs 1 vals

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

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float a) = return a
unpackFloat (List [n]) = unpackFloat n
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat

unpackComplex :: LispVal -> ThrowsError (Complex Double)
unpackComplex (Complex a) = return a
unpackComplex (List [n]) = unpackComplex n
unpackComplex notCmplx = throwError $ TypeMismatch "complex" notCmplx

unpackRational :: LispVal -> ThrowsError (Ratio Integer)
unpackRational (Rational a) = return a
unpackRational (List [n]) = unpackRational n
unpackRational notRa = throwError $ TypeMismatch "rational" notRa

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
      -> (a -> a -> a)              -- Binary op to apply to the list
      -> [LispVal]                  -- List of LispVals to fold the op over
      -> ThrowsError LispVal
binop _ _ _     []  = throwError $ NumArgs 2 []
binop _ _ _ val@[_] = throwError $ NumArgs 2 val
binop unpacker t op p = mapM unpacker p >>= return . t . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -- LispVal Unpacking function
          -> ([Bool] -> Bool)           -- fn to flatten a list of bools
          -> (a -> a -> Bool)           -- the fn to map over the list
          -> [LispVal]                  -- values to be bool'd
          -> ThrowsError LispVal
boolBinop _ _ _     []  = throwError $ NumArgs 2 []
boolBinop _ _ _ val@[_] = throwError $ NumArgs 2 val
boolBinop un bfn fn p =
    mapM unpacker (tupler p) >>=
    return . Bool . bfn . map (\(x,y) -> fn x y)
  where
    -- Make pairs out of the input list
    tupler [] = error "Can't Tuple empty list"
    tupler [_] = []
    tupler (x:y:ys) = (x, y):tupler(y:ys)
    -- Unpack LispVal tuples into a new tuple of vals
    unpacker (x, y) = do
        x1 <- un x
        y1 <- un y
        return (x1, y1)

-- Used to extract the value of an evaluated lisp expression
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "Can only extract Right val"

main :: IO ()
main = do
    args <- S.getArgs
    let prog = args !! 0
    env <- primitiveBindings
    evaled <- runIOThrows $ liftM show $ (liftThrows $ readExpr prog) >>= eval env
    putStrLn $ evaled
