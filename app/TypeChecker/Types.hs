module TypeChecker.Types where

import qualified AbsLatte                   as Abs
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map                   as Map

-- TypeChecker

data Type = Int | String | Bool | Void | Fun [Type] Type Env | Infer
  deriving (Eq, Show)

type Result = (Type, Env)

type TypeChecker = R.ReaderT Env (E.ExceptT Err IO)

class Check a where
  check :: a -> TypeChecker Result

class Check a => CheckList a where
  checkList :: [a] -> TypeChecker [Result]

-- Memory

type Env = Map.Map Abs.Ident Type

-- Err

data Err = GeneralTypeCheckError Abs.BNFC'Position
         | InferError Abs.BNFC'Position
         | MismatchError Type Type Abs.BNFC'Position
         | NoMainError Abs.BNFC'Position
