module Evaluator.Helpers where

import qualified AbsLatte as Abs
import Control.Monad.IO.Class (MonadIO (liftIO))
import Evaluator.Memory (ask, initEnv)
import Evaluator.Types

print :: String -> Interpreter ()
print s = liftIO (putStrLn s)

evalValue :: (Eval a) => a -> Interpreter Val
evalValue x = fst <$> eval x

returnValue :: Val -> Interpreter Result
returnValue v = (,) v <$> ask

returnEnv :: Env -> Interpreter Result
returnEnv env = return (Null, env)

emptyPosition :: Abs.BNFC'Position
emptyPosition = Nothing

emptyBlock :: Abs.Block
emptyBlock = Abs.Block emptyPosition []

emptyFunction :: Val
emptyFunction = Fun [] (Abs.Void emptyPosition) emptyBlock initEnv
