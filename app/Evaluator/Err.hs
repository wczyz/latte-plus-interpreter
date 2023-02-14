module Evaluator.Err where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import Evaluator.Types

throwE :: Err -> Interpreter a
throwE e = lift (lift (E.throwE e))
