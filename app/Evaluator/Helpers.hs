module Evaluator.Helpers where

import qualified AbsLatte                   as Abs
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State
import           Evaluator.Memory
import           Evaluator.Types

print :: String -> Interpreter ()
print s = liftIO (putStrLn s)

evalValue :: Eval a => a -> Interpreter Val
evalValue x = do
  (v, _) <- eval x
  return v

returnValue :: Val -> Interpreter Result
returnValue v = do
  env <- ask
  return (v, env)

returnEnv :: Env -> Interpreter Result
returnEnv env = return (Null, env)

emptyPosition :: Abs.BNFC'Position
emptyPosition = Nothing

instance Show Val where
  show (Int v)    = show v
  show (String s) = s
  show (Bool b)   = show b
  show _          = ""
