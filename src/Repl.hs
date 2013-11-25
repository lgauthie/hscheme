module Repl where

import LispData

import Control.Monad.Error
import System.IO

import qualified System.Console.Readline as R

import Eval (readExpr, eval, runIOThrows, liftThrows, primitiveBindings)

import qualified System.Environment as S

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr)
                  >>= eval env

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: Env -> IO ()
runRepl envRef = do
    result <- R.readline "::> "
    case result of
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just ":q"   -> return ()
        Just r      -> R.addHistory r >> evalAndPrint envRef r >> runRepl envRef
        _           -> runRepl envRef

main :: IO ()
main = do
    args <- S.getArgs
    case length args of
        0 -> primitiveBindings >>= runRepl
        1 -> runOne $ args !! 0
        _ -> putStrLn "Program takes only 0 or 1 argument"
