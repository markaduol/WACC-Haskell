module Codegen where

-- TODO: Completely revamp... again.

import Data.Map as Map
import Control.Monad.State
import Syntax

data Instruction
  = SeqInstr Instruction Instruction
  | Label Label
  | Load Ident Operand
  | Store Atom Ident
  | Jmp Label
  | Call_I Ident Ident [Ident]
  | Return_I Ident
  deriving (Show)

data Operand
  = Atom Atom
  | UnApp_I UnOp_I Atom
  | BinApp_I BinOp_I Atom Atom
  | Memory Atom
  deriving (Show)

data Atom
  = Var Ident
  | Int_I Int
  deriving (Show)

data UnOp_I = I

type Label = String

--------------------------------------------------------------------------------
----------------------------CODE GENERATOR STATE--------------------------------
data CodegenState
  = CodegenState
  { vtable :: SymbolTable
  , ftable :: SymbolTable
  , tempVar :: Ident
  , tempVarCount :: Int
  , instructions :: Instruction } deriving (Show)

-- Isomorphism between 'Codegen a' and 'State CodegenState a'
newtype Codegen a = Codegen {runCodegen :: State CodegenState a} deriving (Show)

instance Functor Codegen where
  fmap f k = k >>= (pure . f)

instance Applicative Codegen where
  pure    = Codegen . pure
  j <*> k = j >>= \f ->
            k >>= (pure . f)

instance Monad Codegen where
  return   = pure
  cg >>= f = f . (fst $ \cgst -> runState (runCodegen cg) cgst)

getVar :: Codegen Ident
getVar = do
  var <- gets tempVar
  count <- tempVarCount
  modify $ \s -> s {tempVarCount = count + 1}
  return var
