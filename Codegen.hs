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
-- intermediate language
type VTable = Map.Map Ident (String, Int)

-- Symbol table mapping function identifiers in our AST to variables in
-- our intermediate language
type FTable = Map.Map Function (String, Int)

-- Our name supply for temporary variables
-- Our temporary variables are of the form t0, t1, t2, x1, y3, etc.
type TempVar = (String, Int)

data BlockState
  = BlockState
  { stack :: [Instruction] } deriving (Show)

data CodegenState
  = CodegenState
  { currentBlock :: BlockState
  , blocks :: [BlockState]
  } deriving (Show)

newtype Codegen a = {runCodegen :: State CodegenState a} deriving (MonadState)

instance Functor Codegen where
  fmap f k = k >>= (pure . f)

instance Applicative Codegen where
  f <*> k = f >>= \a -> fmap a k

instance Monad Codegen where
  return  = pure
  k >>= f = get >>= f . (evalState (runCodegen k))
