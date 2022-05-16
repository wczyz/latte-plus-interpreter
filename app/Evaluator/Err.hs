module Evaluator.Err where

import qualified AbsLatte                   as Abs
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State
import           Evaluator.Types

throwE :: Err -> Interpreter a
throwE e = lift (lift (E.throwE e))

printPosition :: Abs.BNFC'Position -> String
printPosition Nothing = ""
printPosition (Just (row, column)) = "Error in row " ++ show row ++ " and column " ++ show column ++ "\n"

instance Show Err where
  show (DivisionByZero pos) = printPosition pos ++ "Division by zero"
  show (ModuloByZero pos)   = printPosition pos ++ "Modulo by zero"

-- catchE :: Interpreter a -> (Err -> Interpreter a) -> Interpreter a
-- catchE = liftCatch (R.liftCatch E.catchE)
