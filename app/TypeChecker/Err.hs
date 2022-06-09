module TypeChecker.Err where

import qualified AbsLatte                   as Abs
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import           TypeChecker.Types

throwE :: Err -> TypeChecker a
throwE e = lift (E.throwE e)

printPosition :: Abs.BNFC'Position -> String
printPosition Nothing = ""
printPosition (Just (row, column)) = "Error in row " ++ show row ++ " and column " ++ show column ++ "\n"

instance Show Err where
  show (GeneralTypeCheckError str pos) = printPosition pos ++ str
  show (InferError pos)            = printPosition pos ++ "Could not infer type"
  show (MismatchError t1 t2 pos)   = printPosition pos ++ "Mismatched types: expected "
                                                       ++ show t1 ++ " got " ++ show t2
  show (UndefinedName (Abs.Ident str) pos)      = printPosition pos ++ "Undefined name " ++ show str
  show (InvalidNumberOfArguments (Abs.Ident str) pos) = printPosition pos ++ "Invalid number of arguments in function "
                                                        ++ show str
  show (NoMainError pos)           = printPosition pos ++ "Main function not defined"
