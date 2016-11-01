module Codegen where

import Data.Map as Map

-- The semantics of Haskell allow for easy recursion on tree data
-- structures; as we traverse the AST we will generate our code in a
-- recursive, top-down manner.
-- Rather than just using a list to represent our final stack of
-- instructions, we will use a state monad that holds a list of blocks
-- and each block will hold a stack of instructions.
-- We will also use the state monad to append instructions to the stack
-- in blocks.

-- Symbol table mapping identifiers in our AST to variables in our
-- intermediate language.
type SymbolTable = [(String, String)]

data TempVar     = TempVar Int

data BlockState
  = BlockState
  { stack :: [Instruction] } deriving (Show)

-- The first (i.e: largest) constituent unit of our intermediate list of
-- instructions will be sections of intermediate code for each of our functions.
-- Since the implementation of functions is independent (which is what allows
-- us to define 'extern' functions), each section of intermediate code for
-- functions can be independently implemented.

data CodegenState
  = CodegenState
  { currentBlock :: Name -- The block on which we are working.
  , blocks :: [BlockState] -- The list of blocks in the function scope.
  , vtable :: SymbolTable
  , ftable :: SymbolTable
  , tempVar :: TempVar -- The next temporary variable to use.
  } deriving (Show)

newtype Codegen a = {runCodegen :: State CodegenState a} deriving (Functor, Applicative, Monad, MonadState CodegenState, Show)
