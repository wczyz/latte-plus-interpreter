module Evaluator.Types where

import qualified AbsLatte                   as Abs
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map

-- Interpreter

data Val = Int Integer | String String | Bool Bool | Unit | Null | Fun [Abs.Arg] Abs.Type Abs.Block Env
  deriving Eq

type Result = (Val, Env)

type Interpreter = StateT Store (R.ReaderT Env (E.ExceptT Err IO))

class Eval a where
  eval :: a -> Interpreter Result

-- Memory

type Loc = Int

type Memory = Map.Map Loc Val

type Store = (Memory, Loc)

type Env = Map.Map Abs.Ident Loc

-- Err

data Err = DivisionByZero Abs.BNFC'Position | ModuloByZero Abs.BNFC'Position
