module Main where

import qualified AbsLatte                as Abs
import           ErrM
import qualified Evaluator.Err
import           Evaluator.Interpreter   (runInterpreter)
import           GHC.Base                (IO)
import           GHC.IO.FD               (openFile)
import           GHC.IO.IOMode           (IOMode (ReadMode))
import           LexLatte
import           ParLatte
import           System.Environment
import           System.Exit             (exitFailure)
import           System.IO               (hPrint, stderr)
import qualified TypeChecker.Err
import           TypeChecker.TypeChecker (runTypeChecker)

main :: IO ()
main = do
  args <- getArgs
  contents <- case args of
    []           -> getContents
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
  checked <- runTypeChecker program
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
