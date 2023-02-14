module TypeChecker.Memory where

import qualified AbsLatte as Abs
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map as Map
import TypeChecker.Err (throwE)
import TypeChecker.Types

ask :: TypeChecker Env
ask = R.ask

local :: (Env -> Env) -> TypeChecker a -> TypeChecker a
local = R.local

initEnv :: Env
initEnv =
  Map.fromList
    [ (Abs.Ident "printInt", Fun [Int] Void initEnv),
      (Abs.Ident "printString", Fun [String] Void initEnv),
      (Abs.Ident "printBool", Fun [Bool] Void initEnv)
    ]

saveType :: Abs.Ident -> Type -> TypeChecker Env
saveType ident t = R.asks $ Map.insert ident t

getType :: Abs.Ident -> Abs.BNFC'Position -> TypeChecker Type
getType ident pos = do
  env <- ask
  case Map.lookup ident env of
    Just t -> return t
    Nothing -> throwE $ UndefinedName ident pos
