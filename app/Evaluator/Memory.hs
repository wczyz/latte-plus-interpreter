module Evaluator.Memory where

import qualified AbsLatte as Abs
import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Evaluator.Err as E
import Evaluator.Types

ask :: Interpreter Env
ask = lift R.ask

local :: (Env -> Env) -> Interpreter a -> Interpreter a
local f = mapStateT (R.local f)

initStore :: Store
initStore = (Map.empty, 0)

initEnv :: Env
initEnv = Map.empty

allocate :: Abs.Ident -> Val -> Interpreter Env
allocate ident v = do
  env <- ask
  (memory, c) <- get
  put (Map.insert c v memory, c + 1)
  return $ Map.insert ident c env

getValue :: Abs.Ident -> Abs.BNFC'Position -> Interpreter Val
getValue ident pos = do
  env <- ask
  (memory, _) <- get
  let maybeV = Map.lookup ident env >>= (`Map.lookup` memory)
  case maybeV of
    Just v -> return v
    Nothing -> E.throwE $ IdentifierNotFound ident pos

changeValue :: Abs.Ident -> Abs.BNFC'Position -> Val -> Interpreter ()
changeValue ident pos v = do
  env <- ask
  (memory, c) <- get
  case Map.lookup ident env of
    Just l -> put (Map.adjust (const v) l memory, c)
    Nothing -> E.throwE $ IdentifierNotFound ident pos

copyEnv :: Env -> Interpreter Env
copyEnv env = do
  let idents = Map.keys env
  evaluated <- local (const env) $ mapM (`getValue` Nothing) idents
  foldM f initEnv $ zip idents evaluated
  where
    f :: Env -> (Abs.Ident, Val) -> Interpreter Env
    f env' (ident, v) = local (const env') (allocate ident v)
