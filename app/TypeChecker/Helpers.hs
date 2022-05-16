module TypeChecker.Helpers where

import qualified AbsLatte               as Abs
import           Control.Monad.IO.Class
import           TypeChecker.Err
import           TypeChecker.Memory
import           TypeChecker.Types

print :: String -> TypeChecker ()
print s = liftIO (putStrLn s)

checkType :: Check a => a -> TypeChecker Type
checkType x = do
  (t, _) <- check x
  return t

returnValue :: Type -> TypeChecker Result
returnValue v = do
  env <- ask
  return (v, env)

returnEnv :: Env -> TypeChecker Result
returnEnv env = return (Void, env)

convertType :: Abs.Type -> Type
convertType (Abs.Int _)        = Int
convertType (Abs.Str _)        = String
convertType (Abs.Bool _)       = Bool
convertType (Abs.Void _)       = Void
convertType (Abs.Fun _ t args) = Fun (map convertType args) (convertType t) initEnv
convertType (Abs.Infer _)      = Infer

assertSameTypes :: Type -> Type -> Abs.BNFC'Position -> Env -> TypeChecker Result
assertSameTypes t1 t2 pos env = if t1 == t2
                               then returnEnv env
                               else returnEnv env
                               -- TODO: uncomment when typechecker will be ready
                               -- else throwE $ MismatchError t1 t2 pos

emptyPosition :: Abs.BNFC'Position
emptyPosition = Nothing
