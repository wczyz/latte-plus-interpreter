module TypeChecker.Helpers where

import qualified AbsLatte as Abs
import Control.Monad.IO.Class (MonadIO (liftIO))
import TypeChecker.Err (throwE)
import TypeChecker.Memory (ask, initEnv)
import TypeChecker.Types

print :: String -> TypeChecker ()
print s = liftIO (putStrLn s)

checkType :: (Check a) => a -> TypeChecker Type
checkType x = fst <$> check x

returnType :: Type -> TypeChecker Result
returnType v = (,) v <$> ask

returnEnv :: Env -> TypeChecker Result
returnEnv env = return (Unit, env)

convertType :: Abs.Type -> Type
convertType (Abs.Int _) = Int
convertType (Abs.Str _) = String
convertType (Abs.Bool _) = Bool
convertType (Abs.Void _) = Void
convertType (Abs.Fun _ t args) = Fun (map convertType args) (convertType t) initEnv
convertType (Abs.Infer _) = Infer

assertSameTypes :: Type -> Type -> Abs.BNFC'Position -> TypeChecker ()
assertSameTypes Infer Infer _ = return ()
assertSameTypes Infer _ pos = throwE $ InferError pos
assertSameTypes t Infer pos = assertSameTypes Infer t pos
assertSameTypes t1 t2 pos =
  if t1 == t2
    then return ()
    else throwE $ MismatchError t1 t2 pos

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
