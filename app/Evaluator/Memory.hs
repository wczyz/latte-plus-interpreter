module Evaluator.Memory where

import qualified AbsLatte                   as Abs
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Evaluator.Types

ask :: Interpreter Env
ask = lift R.ask

local :: (Env -> Env) -> Interpreter a -> Interpreter a
local f = mapStateT (R.local f)

initStore :: Store
initStore = (Map.empty, 0)

initEnv :: Env
initEnv = Map.empty

allocate :: Abs.Ident -> Val -> Interpreter Env
allocate id v = do
  env <- ask
  (memory, c) <- get
  put (Map.insert c v memory, c + 1)
  return $ Map.insert id c env

getValue :: Abs.Ident -> Interpreter Val
getValue id = do
  env <- ask
  (memory, _) <- get
  let Just v = do
        l <- Map.lookup id env
        Map.lookup l memory
  return v

changeValue :: Abs.Ident -> Val -> Interpreter ()
changeValue id v = do
  env <- ask
  (memory, c) <- get
  let Just l = Map.lookup id env
  put (Map.adjust (const v) l memory, c)

copyEnv :: Env -> Interpreter Env
copyEnv env = do
    let idents = Map.keys env
    evaluated <- mapM getValue idents
    foldM f initEnv $ zip idents evaluated
    where f :: Env -> (Abs.Ident, Val) -> Interpreter Env
          f env (id, v) = local (const env) (allocate id v)
