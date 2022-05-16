module TypeChecker.Memory where

import qualified AbsLatte                   as Abs
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map                   as Map
import           TypeChecker.Types

ask :: TypeChecker Env
ask = R.ask

local :: (Env -> Env) -> TypeChecker a -> TypeChecker a
local = R.local

initEnv :: Env
initEnv = Map.empty

saveType :: Abs.Ident -> Type -> TypeChecker Env
saveType id t = R.asks $ Map.insert id t

getType :: Abs.Ident -> TypeChecker Type
getType id = do
  env <- ask
  let Just t = Map.lookup id env
  return t
