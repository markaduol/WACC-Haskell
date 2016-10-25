module Syntax where

import Type

data Program = Program [Function] Stat deriving (Show)

data Function = Function Type Ident [Param] Stat deriving (Show)

data Param = Param Type Ident deriving (Show)

data Stat
  = Skip
  | VarDecl Type Ident AssignR
  | VarRef AssignL AssignR
  | ReadStat AssignL
  | FreeStat Expr
  | Return Expr
  | Exit Expr
  | Print Expr
  | PrintLn Expr
  | If Expr Stat Stat
  | While Expr Stat
  | BeginEnd Stat
  | StatSemiSep Stat Stat
  deriving (Show)

data AssignL
  = AssignLIdent Ident
  | AssignLArrayElem ArrayElem
  | AssignLPairElem PairElem
  deriving (Show)

data AssignR
  = AssignRExpr Expr
  | AssignRLitArray LitArray
  | AssignRNewPair Expr Expr
  | AssignRPairElem PairElem
  | AssignRFunCall Ident [Arg]
  deriving (Show)

type LitArray = [Expr]

data PairElem = Fst Expr | Snd Expr deriving (Show)

type Arg = Expr

--------------------------------------------------------------------------------
---------------------------------TYPE SYNTAX------------------------------------



--------------------------------------------------------------------------------
-------------------------------EXPRESSION SYNTAX--------------------------------

data Expr
  = LInt Int
  | LBool Bool
  | LChar LitChar
  | LString [LitChar]
  | LPair LitPair
  | IdentCon Ident
  | ArrayElemCon ArrayElem
  | UnApp UnOp Expr
  | BinApp BinOp Expr Expr
  deriving (Show)

data UnOp = NOT_ | SignInv | Len | Ord | Chr deriving (Show)

data BinOp = Mul | Div | Mod | Add | Sub | GT_ | GTE_ | LT_ | LTE_ | EQ_ | NEQ_ | AND_ | OR_ deriving (Show)

data LitChar = CustomChar Char | EscapeSeq Char deriving (Show)

type LitPair = String

type Ident = String

data ArrayElem = ArrayElem Ident [Expr] deriving (Show)
