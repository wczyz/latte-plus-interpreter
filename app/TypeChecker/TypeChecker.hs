{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module TypeChecker.TypeChecker where

import qualified AbsLatte                   as Abs
import           Control.Monad
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
    env' <- local (const env) $ saveType id $ Fun argTypes retType env

    (evaluatedRetType, _) <- local (const env') $ check block
    case retType of
      Infer -> do
          env' <- local (const env') $ saveType id $ Fun argTypes evaluatedRetType env
          returnEnv env'
      _     -> do
          assertSameTypes retType evaluatedRetType pos
          returnEnv env'

instance Check Abs.Block where
  check (Abs.Block _ stmts) = do
    v <- checkType stmts
    returnType v

instance Check Abs.Stmt where
  check (Abs.Empty _)          = returnType Unit

  check (Abs.BStmt _ block)    = check block

  check (Abs.FunDecl _ topdef) = check topdef

  check (Abs.Decl _ t items) = do
    let expectedType = convertType t
    env <- ask
    info <- mapM (\case Abs.NoInit pos id ->
                          case expectedType of
                            Infer -> throwE $ InferError pos
                            _     -> return (id, expectedType, pos)
                        Abs.Init pos id e -> do
                          checkedType <- checkType e
                          case expectedType of
                            Infer -> return (id, checkedType, pos)
                            _ -> do
                              assertSameTypes expectedType checkedType pos
                              return (id, checkedType, pos))
           items
    check $ map (\(id, t, _) -> (id, t)) info

  check (Abs.Ass _ id e) = do
    t <- checkType e
    env <- saveType id t
    returnEnv env

  check (Abs.Incr pos id) = do
     t <- getType id pos
     assertSameTypes Int t pos
     ask >>= returnEnv

  check (Abs.Decr pos id) = check (Abs.Incr pos id)

  check (Abs.Ret _ e) = do
    t <- checkType e
    returnType t

  check (Abs.VRet _) = returnType Void

  check (Abs.Cond pos e s) = do
    t <- checkType e
    assertSameTypes Bool t pos
    check s

  check (Abs.CondElse pos e s1 s2) = do
    t <- checkType e
    assertSameTypes Bool t pos
    t1 <- checkType s1
    t2 <- checkType s2
    assertSameTypes t1 t2 pos
    returnType Unit

  check (Abs.While pos e s) = do
    t <- checkType e
    assertSameTypes Bool t pos
    check s

  check (Abs.SExp _ e) = do
    check e
    returnType Unit

instance Check Abs.Expr where
  check (Abs.EVar pos id) = do
    t <- getType id pos
    returnType t

  check (Abs.ELitInt _ _) = returnType Int
  check (Abs.ELitTrue _)  = returnType Bool
  check (Abs.ELitFalse _) = returnType Bool
  check (Abs.EString _ _) = returnType String

  check (Abs.EApp pos id exprs) = do
    let positions = map Abs.hasPosition exprs
    types <- mapM checkType exprs
    fun <- getType id pos
    case fun of
      Fun expectedTypes retType _ -> do
        when (length types /= length expectedTypes) $ throwE $ InvalidNumberOfArguments id pos
        mapM_ (uncurry3 assertSameTypes) $ zip3 types expectedTypes positions
        returnType retType
      _ -> throwE $ GeneralTypeCheckError "Expected a function" pos

  check (Abs.Neg pos e) = do
    t <- checkType e
    assertSameTypes Int t pos
    returnType t

  check (Abs.Not pos e) = do
    t <- checkType e
    assertSameTypes Bool t pos
    returnType t

  check (Abs.EMul _ e1 _ e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    assertSameTypes Int t1 $ Abs.hasPosition e1
    assertSameTypes Int t2 $ Abs.hasPosition e2
    returnType Int

  check (Abs.EAdd _ e1 _ e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    assertSameTypes Int t1 $ Abs.hasPosition e1
    assertSameTypes Int t2 $ Abs.hasPosition e2
    returnType Int

  check (Abs.ERel _ e1 _ e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    assertSameTypes t1 t2 $ Abs.hasPosition e1
    returnType Bool

  check (Abs.EAnd _ e1 e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    assertSameTypes Bool t1 $ Abs.hasPosition e1
    assertSameTypes Bool t2 $ Abs.hasPosition e2
    returnType Bool

  check (Abs.EOr _ e1 e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    assertSameTypes Bool t1 $ Abs.hasPosition e1
    assertSameTypes Bool t2 $ Abs.hasPosition e2
    returnType Bool

  check (Abs.ELambda pos args ret block) = do
    let argTypes = map (convertType . (\case Abs.Arg _ t _ -> t)) args
        retType  = convertType ret
    env <- ask
    (_, env) <- local (const env) $ check $ map (\case Abs.Arg _ t id -> (id, convertType t)) args

    (evaluatedRetType, _) <- local (const env) $ check block
    when (retType /= Infer) $ assertSameTypes retType evaluatedRetType pos
    return (Fun argTypes evaluatedRetType env, env)

instance Check (Abs.Ident, Type) where
  check (id, t) = do
    env <- saveType id t
    returnEnv env

-- List Check

instance Check Abs.TopDef => Check [Abs.TopDef] where
  check (x : xs) = do
    checked@(_, env) <- check x
    case xs of
      [] -> return checked
      _  -> local (const env) (check xs)
  check [] = returnType Unit

instance (Check Abs.Stmt) => Check [Abs.Stmt] where
  check (x : xs) = do
    checked@(t, env) <- check x
    case xs of
      [] -> return checked
      _  -> if t == Unit
               then local (const env) (check xs)
               else return checked
  check [] = returnType Unit

instance Check Abs.Expr => Check [Abs.Expr] where
  check (x : xs) = do
    checked@(_, env) <- check x
    case xs of
      [] -> return checked
      _  -> local (const env) (check xs)
  check [] = returnType Unit

instance Check (Abs.Ident, Type) => Check [(Abs.Ident, Type)] where
  check (x : xs) = do
    checked@(_, env) <- check x
    case xs of
      [] -> return checked
      _  -> local (const env) (check xs)
  check [] = returnType Unit
