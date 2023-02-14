module Evaluator.Types where

import qualified AbsLatte as Abs
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State
import qualified Data.Map as Map

-- Interpreter

data Val
  = Int Integer
  | String String
  | Bool Bool
  | Unit
  | Null
  | Fun [Abs.Arg] Abs.Type Abs.Block Env
  deriving (Eq)

instance Show Val where
  show (Int v) = show v
  show (String s) = s
  show (Bool b) = show b
  show _ = ""

type Result = (Val, Env)

type Interpreter = StateT Store (R.ReaderT Env (E.ExceptT Err IO))

class Eval a where
  eval :: a -> Interpreter Result

  evalList :: [a] -> Interpreter Result
  evalList (x : xs) = do
    evaluated@(_, env) <- eval x
    case xs of
      [] -> return evaluated
      _ -> mapStateT (R.local (const env)) (evalList xs)
  evalList [] = (,) Null <$> lift R.ask

-- Memory

type Loc = Int

type Memory = Map.Map Loc Val

type Store = (Memory, Loc)

type Env = Map.Map Abs.Ident Loc

-- Err

data Err
  = DivisionByZero Abs.BNFC'Position
  | ModuloByZero Abs.BNFC'Position
  | IdentifierNotFound Abs.Ident Abs.BNFC'Position

printPosition :: Abs.BNFC'Position -> String
printPosition Nothing = ""
printPosition (Just (row, column)) = "Error in row " ++ show row ++ " and column " ++ show column ++ "\n"

instance Show Err where
  show (DivisionByZero pos) = printPosition pos ++ "Division by zero"
  show (ModuloByZero pos) = printPosition pos ++ "Modulo by zero"
  show (IdentifierNotFound (Abs.Ident name) pos) = printPosition pos ++ "Identifier " ++ name ++ " not found"
