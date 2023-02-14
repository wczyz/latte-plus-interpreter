module Main where

import qualified AbsLatte as Abs
import Evaluator.Interpreter (runInterpreter)
import ParLatte
import System.Environment
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import TypeChecker.TypeChecker (Program (Program), runTypeChecker)

main :: IO ()
main = do
  args <- getArgs
  contents <- case args of
    [] -> getContents
    filename : _ -> readFile filename
  program <- parse contents
  handleTypeChecker program
  handleInterpreter program

parse :: String -> IO Abs.Program
parse contents = case pProgram (myLexer contents) of
  Right program -> return program
  Left s -> do
    putStrLn "Parsing error"
    putStrLn s
    exitFailure

handleTypeChecker :: Abs.Program -> IO ()
handleTypeChecker program = do
  checked <- runTypeChecker (Program program)
  case checked of
    Right _ -> return ()
    Left err -> do
      hPrint stderr err
      exitFailure

handleInterpreter :: Abs.Program -> IO ()
handleInterpreter program = do
  result <- runInterpreter program
  case result of
    Right _ -> return ()
    Left err -> do
      hPrint stderr err
      exitFailure
