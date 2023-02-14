module TypeChecker.Err where

import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as E
import TypeChecker.Types

throwE :: Err -> TypeChecker a
throwE e = lift (E.throwE e)
