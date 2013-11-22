module Repl where

import Control.Monad.Error
import System.IO

import qualified System.Console.Readline as R

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

runRepl :: IO ()
runRepl = do
    result <- R.readline "::> "
    case result of
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just ":q" -> return ()
        Just r      -> R.addHistory r >> evalAndPrint r >> runRepl
        _           -> return ()

main :: IO ()
main = do args <- Sys.getArgs
          case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              _ -> putStrLn "Program takes only 0 or 1 argument"
