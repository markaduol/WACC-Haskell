module Codegen where

type Label = String

type FTable = [(Function, Label)]
type VTable = [(Ident, String)]

data CodegenState
  = CodegenState
  { ftable :: FTable -- Symbol table mapping function names to Labels
  , currentBlock :: BlockState -- Current Block to append to
  , vtable :: VTable -- Mapping of variable names to temporary code vars
  }

data BlockState
  = BlockState
  { stack :: [ICode]
  , id :: Int -- Id of the block
  } 

data ICode
  = Add Operand Operand
  | Sub Operand Operand
  | Mul Operand Operand
  | Div Operand Operand
  | Load Operand Operand
  | Store Operand Operand
  | Push Operand
  | Pop Operand
  | Cmp Operand Operand
  | JGE Label
  | JLE Label
  | Jmp Label

data Operand
  = ImmConst Int
  | Reg Int

data BlockState
  = BlockState
  { stack :: [ICode] } deriving (Show)

transExpr :: Expr -> Operand -> [ICode]
transExpr (LInt n) place = Load place (ImmConst n)

transOp Add = [Add]

transStat :: Stat -> [ICode]
transStat (If e st1 st2) =
