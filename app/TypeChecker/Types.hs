module TypeChecker.Types where

import qualified AbsLatte                   as Abs
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import           Data.List                  (intercalate)
import qualified Data.Map                   as Map

-- TypeChecker

data Type = Int | String | Bool | Void | Fun [Type] Type Env | Infer | Unit

-- Not deriving Eq instance because of customized implementation for functions
instance Eq Type where
    (==) Int Int                               = True
    (==) String String                         = True
    (==) Bool Bool                             = True
    (==) Void Void                             = True
    (==) Infer Infer                           = True
    (==) Unit Unit                             = True
    (==) (Fun argsA retA _) (Fun argsB retB _) = retA == retB &&
                                                 length argsA == length argsB &&
                                                 and (zipWith (==) argsA argsB)
    (==) _ _                                   = False

-- Not deriving Show instance because of customized implementation for functions
instance Show Type where
    show Int = "Int"
    show String = "String"
    show Bool = "Bool"
    show Void = "Void"
    show Infer = "Infer"
    show Unit = "Unit"
    show (Fun args ret _) = "Fun " ++ show ret ++ " (" ++ intercalate ", " (map show args) ++ ")"

type Result = (Type, Env)

type TypeChecker = R.ReaderT Env (E.ExceptT Err IO)

class Check a where
  check :: a -> TypeChecker Result

-- Memory

type Env = Map.Map Abs.Ident Type

-- Err

data Err = GeneralTypeCheckError String Abs.BNFC'Position
         | InferError Abs.BNFC'Position
         | MismatchError Type Type Abs.BNFC'Position
         | UndefinedName Abs.Ident Abs.BNFC'Position
         | InvalidNumberOfArguments Abs.Ident Abs.BNFC'Position
         | NoMainError Abs.BNFC'Position
