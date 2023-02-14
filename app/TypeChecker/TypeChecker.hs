{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeChecker where

import qualified AbsLatte as Abs
import Control.Monad (when)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map as Map
import TypeChecker.Err
import TypeChecker.Helpers
import TypeChecker.Memory
import TypeChecker.Types

newtype Program = Program Abs.Program

newtype TopDef = TopDef Abs.TopDef

newtype FnDef = FnDef Abs.TopDef

newtype Block = Block Abs.Block

newtype Stmt = Stmt Abs.Stmt

newtype Expr = Expr Abs.Expr

newtype Pair = Pair (Abs.Ident, Type)

runTypeChecker :: Program -> IO (Either Err Result)
runTypeChecker program = E.runExceptT $ R.runReaderT (typecheck program) initEnv

typecheck :: Program -> TypeChecker Result
typecheck program@(Program (Abs.Program pos _)) = do
  (_, env) <- check program
  let mainIdent = Abs.Ident "main"
      mainExpr = Abs.EApp pos mainIdent []
  if Map.member mainIdent env
    then local (const env) (check (Expr mainExpr))
    else throwE (NoMainError pos)

instance Check Program where
  check (Program (Abs.Program _ topdefs)) = checkList $ map TopDef topdefs

instance Check TopDef where
  check (TopDef (Abs.FnDef pos ret ident args block)) = do
    let argTypes = map (convertType . (\case Abs.Arg _ t _ -> t)) args
        retType = convertType ret
    (_, env) <- checkList $ map (Pair . \case Abs.Arg _ t argIdent -> (argIdent, convertType t)) args
    env' <- local (const env) $ saveType ident $ Fun argTypes retType env

    (evaluatedRetType, _) <- local (const env') $ check (Block block)
    case retType of
      Infer ->
        local (const env') (saveType ident $ Fun argTypes evaluatedRetType env) >>= returnEnv
      _ -> do
        assertSameTypes retType evaluatedRetType pos
        returnEnv env'

instance Check Block where
  check (Block (Abs.Block _ stmts)) =
    checkList (map Stmt stmts) >>= returnType . fst

instance Check Stmt where
  check (Stmt (Abs.Empty _)) = returnType Unit
  check (Stmt (Abs.BStmt _ block)) = check (Block block)
  check (Stmt (Abs.FunDecl _ topdef)) = check (TopDef topdef)
  check (Stmt (Abs.Decl _ t items)) = do
    let expectedType = convertType t
    info <-
      mapM
        ( \case
            Abs.NoInit pos ident ->
              case expectedType of
                Infer -> throwE $ InferError pos
                _ -> return (ident, expectedType, pos)
            Abs.Init pos ident e -> do
              checkedType <- checkType (Expr e)
              -- case expectedType of
              --   Infer -> return ()
              --   _ ->
              when (expectedType /= Infer) $ assertSameTypes expectedType checkedType pos
              return (ident, checkedType, pos)
        )
        items
    checkList $ map (Pair . \(ident, tp, _) -> (ident, tp)) info
  check (Stmt (Abs.Ass _ ident e)) = do
    t <- checkType (Expr e)
    env <- saveType ident t
    returnEnv env
  check (Stmt (Abs.Incr pos ident)) = do
    t <- getType ident pos
    assertSameTypes Int t pos
    ask >>= returnEnv
  check (Stmt (Abs.Decr pos ident)) = check (Stmt (Abs.Incr pos ident))
  check (Stmt (Abs.Ret _ e)) = checkType (Expr e) >>= returnType
  check (Stmt (Abs.VRet _)) = returnType Void
  check (Stmt (Abs.Cond pos e s)) = do
    t <- checkType (Expr e)
    assertSameTypes Bool t pos
    check (Stmt s)
  check (Stmt (Abs.CondElse pos e s1 s2)) = do
    t <- checkType (Expr e)
    assertSameTypes Bool t pos
    t1 <- checkType (Stmt s1)
    t2 <- checkType (Stmt s2)
    assertSameTypes t1 t2 pos
    returnType Unit
  check (Stmt (Abs.While pos e s)) = do
    t <- checkType (Expr e)
    assertSameTypes Bool t pos
    check (Stmt s)
  check (Stmt (Abs.SExp _ e)) =
    check (Expr e) >> returnType Unit

  checkList (x : xs) = do
    checked@(t, env) <- check x
    case xs of
      [] -> return checked
      _ ->
        if t == Unit
          then local (const env) (checkList xs)
          else return checked
  checkList [] = returnType Unit

typeCheckBinOp :: Abs.Expr -> Abs.Expr -> Type -> TypeChecker Result
typeCheckBinOp e1 e2 t = do
  t1 <- checkType (Expr e1)
  t2 <- checkType (Expr e2)
  assertSameTypes t t1 $ Abs.hasPosition e1
  assertSameTypes t t2 $ Abs.hasPosition e2
  returnType t

instance Check Expr where
  check (Expr (Abs.EVar pos ident)) = getType ident pos >>= returnType
  check (Expr (Abs.ELitInt _ _)) = returnType Int
  check (Expr (Abs.ELitTrue _)) = returnType Bool
  check (Expr (Abs.ELitFalse _)) = returnType Bool
  check (Expr (Abs.EString _ _)) = returnType String
  check (Expr (Abs.EApp pos ident exprs)) = do
    let positions = map Abs.hasPosition exprs
    types <- mapM (checkType . Expr) exprs
    funType <- getType ident pos
    case funType of
      Fun expectedTypes retType _ -> do
        when (length types /= length expectedTypes) $ throwE (InvalidNumberOfArguments ident pos)
        mapM_ (uncurry3 assertSameTypes) $ zip3 types expectedTypes positions
        returnType retType
      _ -> throwE $ GeneralTypeCheckError "Expected a function" pos
  check (Expr (Abs.Neg pos e)) = do
    t <- checkType (Expr e)
    assertSameTypes Int t pos
    returnType t
  check (Expr (Abs.Not pos e)) = do
    t <- checkType (Expr e)
    assertSameTypes Bool t pos
    returnType t
  check (Expr (Abs.EMul _ e1 _ e2)) = typeCheckBinOp e1 e2 Int
  check (Expr (Abs.EAdd _ e1 _ e2)) = typeCheckBinOp e1 e2 Int
  check (Expr (Abs.ERel _ e1 _ e2)) = do
    t1 <- checkType (Expr e1)
    t2 <- checkType (Expr e2)
    assertSameTypes t1 t2 $ Abs.hasPosition e1
    returnType Bool
  check (Expr (Abs.EAnd _ e1 e2)) = typeCheckBinOp e1 e2 Bool
  check (Expr (Abs.EOr _ e1 e2)) = typeCheckBinOp e1 e2 Bool
  check (Expr (Abs.ELambda pos args ret block)) = do
    let argTypes = map (convertType . (\case Abs.Arg _ t _ -> t)) args
        retType = convertType ret
        identTypePairs = map (Pair . \(Abs.Arg _ t ident) -> (ident, convertType t)) args

    env <- ask
    (_, env') <- local (const env) $ checkList identTypePairs

    (evaluatedRetType, _) <- local (const env') (check (Block block))
    when (retType /= Infer) $ assertSameTypes retType evaluatedRetType pos
    return (Fun argTypes evaluatedRetType env', env')

instance (Check Pair) where
  check (Pair (ident, t)) = saveType ident t >>= returnEnv
