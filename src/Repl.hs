module Repl where

import Control.Monad.Error
import System.IO

import Eval (extractValue, trapError, readExpr, eval)

import qualified System.Environment as Sys

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr = return $ extractValue
                $ trapError (liftM show $ readExpr expr >>= eval)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ cond prompt action = do
    result <- prompt
    if cond result
        then return ()
        else action result >> until_ cond prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do args <- Sys.getArgs
          case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              _ -> putStrLn "Program takes only 0 or 1 argument"
