{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SemanticAnalyser where

import Data.Map as Map

import AST_Traversal

-- We maintain a reference to the enclosing symbol table.
data SymbolTable
  = SymbolTable (Map.Map Ident Type) SymbolTable
  | None -- No enclosing symbol table

data SemCheckerState
  = SemCheckerState
  { vtable :: SymbolTable
  , ftable :: SymbolTable
  }

newtype SemCheck a = SemCheck {runSemCheck :: State SemCheckerState a}
  deriving (MonadState SemCheckerState)

checkExpr :: (Monad m) => SemCheckerState -> Expr -> m (Either Error Type)
checkExpr (LInt n) = Right tyInt
checkExpr (LBool b) = Right tyBool
checkExpr (LChar c) = Right tyChar
checkExpr (LString cs) = Right tyString
checkExpr (BinApp binop ex1 ex2) = do
  t1 <- checkExpr ex1
  t2 <- checkExpr ex2
  if (t1 == tyInt) && (t2 == tyInt)
    then return tyInt
    else error ("Type mismatch, expecting: " ++ tyNameInt)
