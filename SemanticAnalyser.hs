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

checkStmt

checkExpr :: (Monad m) => SemCheck a -> Expr -> SemCheck Type
checkExpr _ (LInt n)     = pure tyInt
checkExpr _ (LBool b)    = pure tyBool
checkExpr _ (LChar c)    = pure tyChar
checkExpr _ (LString cs) = pure tyString
checkExpr _ (LPair p)    = pure tyPair

checkExpr semCheck (ArrayElemExpr (ArrayElem i exs)) = do
  t <- checkExpr semCheck (Ident i)
  if t /= tyArr
    then error ("\nIdentifier: " ++ i ++ " is not bound as an array type.")
    else mapM f exs
  where
    f semCheck ex = do
      t <- checkExpr semCheck ex
      if t /= tyInt
        then error ("\nArray index expression: " ++ (show ex)
        ++ " is not bound as an int type.")
        else return t

checkExpr semCheck (UnApp NOT_ ex) = do
  t <- checkExpr semCheck ex
  if t == tyBool
    then return tyBool
    else error ("\nType mismatch, expecting: " ++ tyNameBool ++ "\nin expression " ++ (show ex))
checkExpr semCheck (UnApp SignInv ex) = do
  t <- checkExpr semCheck ex
  if t == tyInt
    then return tyInt
    else error ("\nType mismatch, expecting: " ++ tyNameInt ++ "\nin expression " ++ (show ex))
checkExpr semCheck (UnApp Len ex) = do
  t <- checkExpr semCheck ex
  if t == tyArr
    then return tyArr
    else error ("\nType mismatch, expecting:" ++ tyNameArr ++ "\nin expression " ++ (show ex))
checkExpr semCheck (UnApp unop ex)
  | (unop == Ord) || (unop == Chr) = do
    t <- checkExpr semCheck ex
    if t == tyChar
      then return tyChar
      else error ("\nType mismatch, expecting:" ++ tyNameChar ++ "\nin expression " ++ (show ex))
  | otherwise = error ("\nUnrecognized unary operator: " ++ (show unop))

checkExpr semCheck (BinApp binop ex1 ex2) = do
  | elem binop numBinops = do
    t1 <- checkExpr semCheck ex1
    t2 <- checkExpr semCheck ex2
    if (t1 == t2) && (t1 == tyInt)
      then return tyInt
      else error ("\nType mismatch, expecting: " ++ tyNameInt ++
      " for both arguments." ++ "\nin expression " ++ (show ex))
  | elem binop logicBinops = do
    t1 <- checkExpr semCheck ex1
    t2 <- checkExpr semCheck ex2
    if (t1 == t2) && ((t1 == tyInt) || (t1 == tyChar))
      then return t1
      else return ("\nType mismatch, expecting: " ++ tyNameInt ++ " or "
      ++ tyNameChar ++ " for both arguments." ++ "\nin expression " ++ (show ex))
  where
    numBinops   = [Mul, Div, Mod, Add, Sub]
    logicBinops = [GT_, GTE_, LT_, LTE_, EQ_, NEQ_, AND_, OR_]

checkExpr semCheck (Ident i) = do
  (SymbolTable vtab ref) <- gets vtable
  case Map.lookup i vtab of
    Just t  -> return t
    Nothing -> error ("\nIdentifer: " ++ i ++ " is unbound.")
