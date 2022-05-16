{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module TypeChecker.TypeChecker where

import qualified AbsLatte                   as Abs
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map                   as Map
import           TypeChecker.Err
import           TypeChecker.Helpers
import           TypeChecker.Memory
import           TypeChecker.Types

runTypeChecker :: Abs.Program -> IO (Either Err Result)
runTypeChecker prog = E.runExceptT $ R.runReaderT (typecheck prog) initEnv

typecheck :: Abs.Program -> TypeChecker Result
typecheck program@(Abs.Program pos _) = do
    (_, env) <- check program
    let mainIdent = Abs.Ident "main"
        mainExpr  = Abs.EApp pos mainIdent []
    if Map.member mainIdent env
       then local (const env) $ check mainExpr
       else throwE (NoMainError pos)

instance Check Abs.Program where
  check (Abs.Program _ topdefs) = check topdefs

instance Check Abs.TopDef where
  check (Abs.FnDef pos ret id args block) = do
    let argTypes = map (convertType . (\case Abs.Arg _ t _ -> t)) args
        retType  = convertType ret
    (_, env) <- check $ map (\case Abs.Arg _ t id -> (id, convertType t)) args
    env' <- saveType id $ Fun argTypes retType env

    (evaluatedRetType, _) <- check block
    assertSameTypes retType evaluatedRetType pos env'

instance Check Abs.Block where
  check (Abs.Block _ stmts) = do
    env <- ask
    v <- checkType stmts
    return (v, env)

instance Check Abs.Stmt where
  check (Abs.Empty _)          = returnValue Void

  check (Abs.BStmt _ block)    = check block

  check (Abs.FunDecl _ topdef) = check topdef

  check (Abs.Decl _ t items) = do
    let expectedType = convertType t
    env <- ask
    info <- mapM (\case Abs.NoInit pos id -> return (id, expectedType, pos)
                        Abs.Init pos id e -> do
                          checkedType <- checkType e
                          assertSameTypes expectedType checkedType pos env
                          return (id, checkedType, pos))
           items
    check $ map (\(id, t, _) -> (id, t)) info

  check _                      = returnValue Void

instance Check Abs.Expr where
  check e = returnValue Void

instance Check (Abs.Ident, Type) where
  check (id, t) = do
    env <- saveType id t
    returnEnv env

-- instance Check (Abs.Ident, Type) => Check [(Abs.Ident, Type)] where
--   check (x : xs) = do
--     checked@(_, env) <- check x
--     case xs of
--       [] -> return checked
--       _  -> local (const env) (check xs)
--   check [] = returnValue Void

instance Check a => Check [a] where
  check (x : xs) = do
    checked@(_, env) <- check x
    case xs of
      [] -> return checked
      _  -> local (const env) (check xs)
  check [] = returnValue Void

-- instance CheckList Abs.TopDef where
--   checkList (x : xs) = do
--     checked@(_, env) <- check x
--     case xs of
--       [] -> return [checked]
--       _  -> local (const env) (checkList xs)
--   checkList [] = return []
