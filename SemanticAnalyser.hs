{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SemanticAnalyser where

import Data.Map.Strict as Map
import Data.Functor.Identity
import Control.Monad.State
import Type
import Syntax
import AST_Traversal

type SymbolTable = Map.Map Ident Type

data SemCheckerState
  = SemCheckerState
  { vtable :: (SymbolTable, Maybe SymbolTable) -- We maintain a reference to the enclosing symbol table.
  , ftable :: SymbolTable
  }

newtype SemCheck a = SemCheck {runSemCheck :: State SemCheckerState a}
  deriving (MonadState SemCheckerState)

instance Functor SemCheck where
  fmap f k = k >>= (pure . f)

instance Applicative SemCheck where
  pure = SemCheck . pure
  j <*> k = j >>= \f ->
            k >>= (pure . f)

instance Monad SemCheck where
  return = pure
  x >>= k = state $ \s ->
    let (a, s') = runState (runSemCheck x) s
    in runState (runSemCheck (k a)) s'

traverseStat :: SemCheck a -> Stat -> SemCheck Stat
traverseStat _ Skip = pure Skip
traverseStat semCheck (VarDef typ ident assignR) = do
  (vtab, ref) <- gets vtable
  case Map.lookup ident vtab of
    Just x -> error ("\nIdentifier: " ++ ident ++ " is already defined in the current scope.")
    Nothing -> modify $ \s -> s {vtable = (Map.insert ident typ (fst (vtable s)), ref)}

checkExpr :: SemCheck a -> Expr -> SemCheck Type
checkExpr _ (LInt n)     = pure tyInt
checkExpr _ (LBool b)    = pure tyBool
checkExpr _ (LChar c)    = pure tyChar
checkExpr _ (LString cs) = pure tyString
--checkExpr _ (LPair p)    = pure tyPair

{-checkExpr semCheck (ArrayElemExpr (ArrayElem i exs)) = do
  t <- checkExpr semCheck (Ident i)
  if t /= tyArr
    then error ("\nIdentifier: " ++ i ++ " is not bound as an array type.")
    else mapM f exs
  where
    f semCheck ex = do
      t <- checkExpr semCheck ex
      if t /= tyInt
        then error ("\nArray index expression: " ++ (show ex) ++ " is not bound as an int type.")
        else return t-}

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

checkExpr semCheck (BinApp binop ex1 ex2)
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
      else error ("\nType mismatch, expecting: " ++ tyNameInt ++ " or " ++ tyNameChar ++ " for both arguments." ++ "\nin expression " ++ (show (BinApp binop ex1 ex2)))
  where
    numBinops   = [Mul, Div, Mod, Add, Sub]
    logicBinops = [GT_, GTE_, LT_, LTE_, EQ_, NEQ_, AND_, OR_]

checkExpr semCheck (Ident i) = do
  (vtab, ref) <- gets vtable
  case Map.lookup i vtab of
    Just t  -> return t
    Nothing -> error ("\nIdentifer: " ++ i ++ " is unbound.")
