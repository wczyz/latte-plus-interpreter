{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Evaluator.Interpreter where

import qualified AbsLatte as Abs
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State
import Evaluator.Err
import Evaluator.Helpers
import Evaluator.Memory
import Evaluator.Types

runInterpreter :: Abs.Program -> IO (Either Err (Result, Store))
runInterpreter prog = E.runExceptT $ R.runReaderT (runStateT (interpret prog) initStore) initEnv

newtype Program = Program Abs.Program

newtype TopDef = TopDef Abs.TopDef

newtype Block = Block Abs.Block

newtype Stmt = Stmt Abs.Stmt

newtype Item = Item Abs.Item

newtype Expr = Expr Abs.Expr

newtype Pair = Pair (Abs.Ident, Val)

interpret :: Abs.Program -> Interpreter Result
interpret program@(Abs.Program position _) = do
  (_, env) <- eval (Program program)
  let mainIdent = Abs.Ident "main"
      mainExpr = Abs.EApp position mainIdent []
  local (const env) $ eval (Expr mainExpr)

instance Eval Program where
  eval (Program (Abs.Program _ topdefs)) = evalList (map TopDef topdefs)

instance Eval TopDef where
  eval (TopDef (Abs.FnDef pos t ident args b)) = do
    env <- allocate ident emptyFunction >>= copyEnv
    local (const env) (changeValue ident pos (Fun args t b env))
    returnEnv env

instance Eval Block where
  eval (Block (Abs.Block _ stmts)) =
    evalList (map Stmt stmts) >>= returnValue . fst

instance Eval Stmt where
  eval (Stmt (Abs.Empty _)) = returnValue Null
  eval (Stmt (Abs.BStmt _ block)) = eval (Block block)
  eval (Stmt (Abs.FunDecl _ topdef)) = eval (TopDef topdef)
  eval (Stmt (Abs.Decl _ _ items)) = evalList (map Item items)
  eval (Stmt (Abs.Ass pos ident e)) = do
    v <- evalValue (Expr e)
    changeValue ident pos v
    returnValue Null
  eval (Stmt (Abs.Incr pos ident)) = do
    Int v <- getValue ident pos
    changeValue ident pos (Int (v + 1))
    returnValue Null
  eval (Stmt (Abs.Decr pos ident)) = do
    Int v <- getValue ident pos
    changeValue ident pos (Int (v - 1))
    returnValue Null
  eval (Stmt (Abs.Ret _ e)) = eval (Expr e)
  eval (Stmt (Abs.VRet _)) = returnValue Unit
  eval (Stmt (Abs.Cond _ e s)) = do
    Bool b <- evalValue (Expr e)
    if b
      then eval (Stmt s)
      else returnValue Null
  eval (Stmt (Abs.CondElse _ e s1 s2)) = do
    Bool b <- evalValue (Expr e)
    if b then eval (Stmt s1) else eval (Stmt s2)
  eval loop@(Stmt (Abs.While _ e s)) = do
    Bool b <- evalValue (Expr e)
    if b
      then eval (Stmt s) >> eval loop
      else returnValue Null
  eval (Stmt (Abs.SExp _ e)) = eval (Expr e) >> returnValue Null

  evalList (x : xs) = do
    evaluated@(v, env) <- eval x
    case xs of
      [] -> return evaluated
      _ ->
        if v == Null
          then local (const env) (evalList xs)
          else return evaluated
  evalList [] = returnValue Null

instance Eval Item where
  eval (Item (Abs.NoInit _ ident)) = allocate ident Null >>= returnEnv
  eval (Item (Abs.Init _ ident e)) = do
    v <- evalValue (Expr e)
    env <- allocate ident v
    returnEnv env

instance Eval Expr where
  eval (Expr (Abs.EVar pos ident)) = getValue ident pos >>= returnValue
  eval (Expr (Abs.ELitInt _ v)) = returnValue (Int v)
  eval (Expr (Abs.ELitTrue _)) = returnValue (Bool True)
  eval (Expr (Abs.ELitFalse _)) = returnValue (Bool False)
  eval (Expr (Abs.EString _ s)) = returnValue (String s)
  eval (Expr (Abs.Neg _ e)) = do
    Int v <- evalValue (Expr e)
    returnValue $ Int (-v)
  eval (Expr (Abs.Not _ e)) = do
    Bool b <- evalValue (Expr e)
    returnValue $ Bool (not b)
  eval (Expr (Abs.EMul _ e1 op e2)) = do
    Int v1 <- evalValue (Expr e1)
    Int v2 <- evalValue (Expr e2)
    case op of
      Abs.Times _ -> returnValue $ Int (v1 * v2)
      Abs.Mod pos ->
        if v2 /= 0
          then returnValue $ Int (v1 `mod` v2)
          else throwE $ ModuloByZero pos
      Abs.Div pos ->
        if v2 /= 0
          then returnValue $ Int (v1 `div` v2)
          else throwE $ DivisionByZero pos
  eval (Expr (Abs.EAdd _ e1 op e2)) = do
    Int v1 <- evalValue (Expr e1)
    Int v2 <- evalValue (Expr e2)
    let result = case op of
          Abs.Plus _ -> v1 + v2
          Abs.Minus _ -> v1 - v2
    returnValue $ Int result
  eval (Expr (Abs.ERel _ e1 op e2)) = do
    Int v1 <- evalValue (Expr e1)
    Int v2 <- evalValue (Expr e2)
    let predicate = case op of
          Abs.LTH _ -> (<)
          Abs.LE _ -> (<=)
          Abs.GTH _ -> (>)
          Abs.GE _ -> (>=)
          Abs.EQU _ -> (==)
          Abs.NE _ -> (/=)
    returnValue $ Bool (predicate v1 v2)
  eval (Expr (Abs.EAnd _ e1 e2)) = do
    Bool b1 <- evalValue (Expr e1)
    Bool b2 <- evalValue (Expr e2)
    returnValue $ Bool (b1 && b2)
  eval (Expr (Abs.EOr _ e1 e2)) = do
    Bool b1 <- evalValue (Expr e1)
    Bool b2 <- evalValue (Expr e2)
    returnValue $ Bool (b1 || b2)
  eval (Expr (Abs.ELambda _ args t block)) = do
    env <- ask >>= copyEnv
    returnValue $ Fun args t block env
  eval (Expr (Abs.EApp pos ident exprs)) = do
    case ident of
      Abs.Ident "printInt" -> printBlock
      Abs.Ident "printString" -> printBlock
      Abs.Ident "printBool" -> printBlock
      _ -> do
        Fun args _ block env <- getValue ident pos
        evaluatedArgs <- mapM (evalValue . Expr) exprs
        let idents = map (\case Abs.Arg _ _ i -> i) args
        (_, env') <- local (const env) (evalList (zipWith (curry Pair) idents evaluatedArgs))
        local (const env') (eval (Block block))
    where
      printBlock = do
        evaluatedArgs <- mapM (evalValue . Expr) $ take 1 exprs
        case evaluatedArgs of
          [] -> Evaluator.Helpers.print ""
          x : _ -> Evaluator.Helpers.print $ show x
        returnValue Null

instance Eval Pair where
  eval (Pair (ident, v)) = allocate ident v >>= returnEnv
