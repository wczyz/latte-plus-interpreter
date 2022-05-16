{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Evaluator.Interpreter where

import qualified AbsLatte                   as Abs
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import           Evaluator.Err
import           Evaluator.Helpers
import           Evaluator.Memory
import           Evaluator.Types

runInterpreter :: Abs.Program -> IO (Either Err (Result, Store))
runInterpreter prog = E.runExceptT $ R.runReaderT (runStateT (interpret prog) initStore) initEnv

interpret :: Abs.Program -> Interpreter Result
interpret program@(Abs.Program position _) = do
  (_, env) <- eval program
  let mainIdent = Abs.Ident "main"
      mainExpr = Abs.EApp position mainIdent []
  local (const env) $ eval mainExpr

instance Eval Abs.Program where
  eval (Abs.Program _ topdefs) = eval topdefs

instance Eval Abs.TopDef where
  eval (Abs.FnDef _ t id args b) = do
    env <- allocate id Null
    env' <- copyEnv env
    local (const env) (changeValue id (Fun args t b env'))
    returnEnv env

instance Eval Abs.Block where
  eval (Abs.Block _ stmts) = do
    env <- ask
    (v, _) <- eval stmts
    return (v, env)

instance Eval Abs.Stmt where
  eval (Abs.Empty _) = returnValue Null

  eval (Abs.BStmt _ block) = eval block

  eval (Abs.FunDecl _ topdef) = eval topdef

  eval (Abs.Decl _ _ items) = eval items

  eval (Abs.Ass _ id e) = do
    v <- evalValue e
    changeValue id v
    returnValue Null

  eval (Abs.Incr _ id) = do
    Int v <- getValue id
    changeValue id (Int (v + 1))
    returnValue Null

  eval (Abs.Decr _ id) = do
    Int v <- getValue id
    changeValue id (Int (v - 1))
    returnValue Null

  eval (Abs.Ret _ e) = eval e

  eval (Abs.VRet _) = returnValue Unit

  eval (Abs.Cond _ e s) = do
    Bool b <- evalValue e
    if b then eval s else returnValue Null

  eval (Abs.CondElse _ e s1 s2) = do
    Bool b <- evalValue e
    if b then eval s1 else eval s2

  eval loop@(Abs.While _ e s) = do
    Bool b <- evalValue e
    if b
      then do
        eval s
        eval loop
      else returnValue Null

  eval (Abs.SExp _ e) = do
    eval e
    returnValue Null

instance Eval Abs.Item where
  eval (Abs.NoInit _ id) = do
    env <- allocate id Null
    returnEnv env

  eval (Abs.Init _ id e) = do
    v <- evalValue e
    env <- allocate id v
    returnEnv env

instance Eval Abs.Expr where
  eval (Abs.EVar _ id) = do
    v <- getValue id
    returnValue v

  eval (Abs.ELitInt _ v) = returnValue (Int v)
  eval (Abs.ELitTrue _)  = returnValue (Bool True)
  eval (Abs.ELitFalse _) = returnValue (Bool False)
  eval (Abs.EString _ s) = returnValue (String s)

  eval (Abs.Neg _ e) = do
    Int v <- evalValue e
    returnValue $ Int (-v)
  eval (Abs.Not _ e) = do
    Bool b <- evalValue e
    returnValue $ Bool (not b)

  eval (Abs.EMul _ e1 op e2) = do
    Int v1 <- evalValue e1
    Int v2 <- evalValue e2
    case op of
          Abs.Times _ -> returnValue $ Int (v1 * v2)
          Abs.Mod pos -> if v2 /= 0
                          then returnValue $ Int (v1 `mod` v2)
                          else throwE $ ModuloByZero pos
          Abs.Div pos -> if v2 /= 0
                          then returnValue $ Int (v1 `div` v2)
                          else throwE $ DivisionByZero pos

  eval (Abs.EAdd _ e1 op e2) = do
    Int v1 <- evalValue e1
    Int v2 <- evalValue e2
    let result = case op of
                   Abs.Plus _  -> v1 + v2
                   Abs.Minus _ -> v1 - v2
    returnValue $ Int result

  eval (Abs.ERel _ e1 op e2) = do
    Int v1 <- evalValue e1
    Int v2 <- evalValue e2
    let b = case op of
              Abs.LTH _ -> v1 < v2
              Abs.LE _  -> v1 <= v2
              Abs.GTH _ -> v1 > v2
              Abs.GE _  -> v1 >= v2
              Abs.EQU _ -> v1 == v2
              Abs.NE _  -> v1 /= v2
    returnValue $ Bool b

  eval (Abs.EAnd _ e1 e2) = do
    Bool b1 <- evalValue e1
    Bool b2 <- evalValue e2
    returnValue $ Bool (b1 && b2)

  eval (Abs.EOr _ e1 e2) = do
    Bool b1 <- evalValue e1
    Bool b2 <- evalValue e2
    returnValue $ Bool (b1 || b2)

  eval (Abs.ELambda _ args t block) = do
    env <- ask
    env' <- copyEnv env
    returnValue $ Fun args t block env'

  eval (Abs.EApp _ id exprs) = do
    case id of
      Abs.Ident "print" -> do
        evaluatedArgs <- mapM evalValue $ take 1 exprs
        case evaluatedArgs of
          []    -> Evaluator.Helpers.print ""
          x : _ -> Evaluator.Helpers.print $ show x
        returnValue Null
      _ -> do
        Fun args t block env <- getValue id
        evaluatedArgs <- mapM evalValue exprs
        let idents = map (\case Abs.Arg _ _ i -> i) args
        (_, env') <- local (const env) (eval $ zip idents evaluatedArgs)
        local (const env') (eval block)

instance Eval (Abs.Ident, Val) where
  eval (id, v) = do
    env <- allocate id v
    returnEnv env

-- List Eval

instance (Eval Abs.Expr) => Eval [Abs.Expr] where
  eval (x : xs) = do
    evaluated@(_, env) <- eval x
    case xs of
      [] -> return evaluated
      _  -> local (const env) (eval xs)
  eval [] = returnValue Null

instance (Eval Abs.TopDef) => Eval [Abs.TopDef] where
  eval (x : xs) = do
    evaluated@(_, env) <- eval x
    case xs of
      [] -> return evaluated
      _  -> local (const env) (eval xs)
  eval [] = returnValue Null

instance (Eval Abs.Item) => Eval [Abs.Item] where
  eval (x : xs) = do
    evaluated@(_, env) <- eval x
    case xs of
      [] -> return evaluated
      _  -> local (const env) (eval xs)
  eval [] = returnValue Null

instance (Eval Abs.Stmt) => Eval [Abs.Stmt] where
  eval (x : xs) = do
    evaluated@(v, env) <- eval x
    case xs of
      [] -> return evaluated
      _  -> if v == Null
               then local (const env) (eval xs)
               else return evaluated
  eval [] = returnValue Null

instance (Eval (Abs.Ident, Val)) => Eval [(Abs.Ident, Val)] where
  eval (x : xs) = do
    evaluated@(v, env) <- eval x
    case xs of
      [] -> return evaluated
      _  -> if v == Null
               then local (const env) (eval xs)
               else return evaluated
  eval [] = returnValue Null
