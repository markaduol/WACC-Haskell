{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Map.Strict as Map
import Data.Functor.Identity
import Control.Monad.State
import Syntax

data Instruction
  = Label Label
  | Load Var Operand
  | Store Atom Var
  | Jmp Label
  | Call_I Var Var [Var]  -- Storage variable; function identifier; list of param variables
  | Return_I Var
  deriving (Show)

data Operand
  = Atom Atom
  | UnApp_I UnOp_I Atom
  | BinApp_I BinOp_I Atom Atom
  | Memory Atom
  deriving (Show)

data Atom
  = Var Var
  | Int_I Int
  deriving (Show)

data UnOp_I = IR_NOT deriving (Show)

data BinOp_I = IR_Add | IR_Mul deriving (Show)

type Label = String

type Name = String

-- We use '[Var]' instead of 'Var' to deal with cases when the same variable is
-- defined twice (or more) in an outer scope and an inner scope
type SymbolTable = Map.Map Ident [Var]

data Var = Named (Ident, Int) | UnNamed Int deriving (Show)

--------------------------------------------------------------------------------
------------------------------- CODEGEN STATE ----------------------------------
data BasicBlock = BasicBlock Name [Instruction] Instruction deriving (Show)

-- We will perform code generation independently for each function. Therefore,
-- the scope of 'CodegenState' is only within a given function.
-- 'blockCount' gives the information necessary to determine the index of
-- the next block to be added (within the function scope).
data CodegenState
  = CodegenState
  { currentBlockName :: Name                    -- Name of current block on which we are working
  , blocks           :: Map.Map Name BlockState -- Map of names to blocks; only 'CodegenState' knows block names.
  , blockCount       :: Int                     -- the number of blocks in the function scope
  , vtable           :: SymbolTable             -- Symbol table for variable identifiers
  , ftable           :: SymbolTable             -- Symbol table for function identifiers
  , tempVarCount     :: Int                     -- Count of temporary (i.e: UnNamed) variables used so far
  } deriving (Show)

data BlockState
  = BlockState
  { index      :: Int               -- index of block within function scope
  , stack      :: [Instruction]     -- Instuctions in the current block
  , terminator :: Maybe Instruction -- Final instruction in block, which is used to end the block
  } deriving (Show)

-- We use the state monad to maintain state throughout code generation
newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (MonadState CodegenState)

instance Functor Codegen where
  fmap f k = k >>= (pure . f)

instance Applicative Codegen where
  pure    = Codegen . pure
  j <*> k = j >>= \f ->
            k >>= (pure . f)

instance Monad Codegen where
  return  = pure
  x >>= k = state $ \s ->
    let (a, s') = runState (runCodegen x) s
    in runState (runCodegen (k a)) s'

makeBlocks :: CodegenState -> [BasicBlock]
makeBlocks s = fmap createBlock (Map.toAscList (blocks s))

-- Basic function to create a block from name and block state
createBlock :: (Name, BlockState) -> BasicBlock
createBlock (name, (BlockState _ ins trm)) = BasicBlock name ins (createTrm trm)
  where
    createTrm :: Maybe Instruction -> Instruction
    createTrm trm = case trm of
      Just x  -> x
      Nothing -> error ("No terminating instruction for block: " ++ (show name))

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState "entryBlock" Map.empty 0 Map.empty Map.empty 0

execCodegen :: Codegen a -> CodegenState
execCodegen cg = execState (runCodegen cg) emptyCodegenState

evalCodegen :: Codegen a -> a
evalCodegen cg = evalState (runCodegen cg) emptyCodegenState

getVar :: Codegen Var
getVar = do
  var <- gets tempVarCount
  modify $ \s -> s {tempVarCount = var + 1}
  return (UnNamed var)

addVarIdent :: Ident -> Codegen Ident
addVarIdent i = do
  vtab <- gets vtable
  case Map.lookup i vtab of
    Just (v:vs) -> let varCount = length (v:vs) in
                   modify $ \s -> s {vtable = Map.insert i ((v:vs) ++ [(Named (i, varCount))]) (vtable s)}
    Nothing     -> modify $ \s -> s {vtable = Map.insert i [(Named (i, 0))] (vtable s)}
  return i

addFuncIdent :: Ident -> Codegen Ident
addFuncIdent i = do
  ftab <- gets ftable
  case Map.lookup i ftab of
    Just (v:vs) -> let varCount = length (v:vs) in
                   modify $ \s -> s {ftable = Map.insert i ((v:vs) ++ [(Named (i, varCount))]) (ftable s)}
    Nothing     -> modify $ \s -> s {ftable = Map.insert i [(Named (i, 0))] (ftable s)}
  return i

--------------------------------------------------------------------------------
---------------------------------- BLOCKS --------------------------------------

-- Whenever creating the block, we must supply the index to ensure that it
-- occupies the right position within the list of blocks in the function scope
emptyBlock :: Int -> BlockState
emptyBlock index = BlockState index [] Nothing

currentBlock :: Codegen BlockState
currentBlock = do
  blkName <- gets currentBlockName
  blks <- gets blocks
  case Map.lookup blkName blks of
    Just res -> return res
    Nothing -> error ("No such block: " ++ (show blkName))

addBlock :: Name -> Codegen Name
addBlock blkName = do
  blks <- gets blocks
  blkCount <- gets blockCount
  let newBlock = emptyBlock blkCount
  modify $ \s -> s { blocks = Map.insert blkName newBlock (blocks s)
                   , blockCount = blkCount + 1 }
  return blkName

setBlock :: Name -> Codegen Name
setBlock blkName = do
  modify $ \s -> s { currentBlockName = blkName }
  return blkName

modifyBlock :: BlockState -> Codegen ()
modifyBlock newBlock = do
  blkName <- gets currentBlockName
  modify $ \s -> s {blocks = Map.insert blkName newBlock (blocks s)}

terminateInstr :: Instruction -> Codegen Instruction
terminateInstr trm = do
  blk <- currentBlock
  modifyBlock $ blk {terminator = Just trm}
  return trm

--------------------------------------------------------------------------------
---------------------------- TRANSLATE EXPRESSIONS -----------------------------

transExprs :: [Expr] -> Codegen [Instruction]
transExprs [expr] = do
  var <- getVar
  transExpr expr var
transExprs (expr:exprs) = do
  code1 <- transExprs [expr]
  code2 <- transExprs exprs
  return (code1 ++ code2)

transExpr :: Expr -> Var -> Codegen [Instruction]
transExpr (LInt n) var = do
  blk <- currentBlock
  let stk = stack blk
  let instr = Load var (Atom (Int_I n))
  modifyBlock $ blk {stack = stk ++ [instr]}
  return [instr]

transExpr (LBool bool) var = do
  blk <- currentBlock
  let stk = stack blk
  let instr = Load var (Atom (Int_I (transBool bool)))
  modifyBlock $ blk {stack = stk ++ [instr]}
  return [instr]
  where
    transBool bool = case bool of
      True  -> 1
      False -> 0

transExpr (Ident ident) var = do
  blk <- currentBlock
  let stk = stack blk
  valSymtab <- gets vtable
  let as = fromMaybe (Map.lookup ident valSymtab)
  let instr = Load var (Atom (Var (last as))) -- We select the last added identifier: i.e, in [a1,...,an] we select 'an'.
  modifyBlock $ blk {stack = stk ++ [instr]}
  return [instr]
  where
    fromMaybe :: Maybe a -> a
    fromMaybe res = case res of
      Just x -> x
      Nothing -> error "Identifier not found in symbol table."

transExpr (UnApp unop ex) var = do
  var1 <- getVar
  code1 <- transExpr ex var1
  op <- transUnOp unop
  blk <- currentBlock
  let stk = stack blk
  let instr = Load var (UnApp_I op (Var var1))
  modifyBlock $ blk {stack = stk ++ [instr]}
  return (code1 ++ [instr])

transExpr (BinApp binop ex1 ex2) var = do
  var1 <- getVar
  var2 <- getVar
  code1 <- transExpr ex1 var1
  code2 <- transExpr ex2 var2
  op <- transBinOp binop
  blk <- currentBlock
  let stk = stack blk
  let instr = Load var (BinApp_I op (Var var1) (Var var2))
  modifyBlock $ blk {stack = stk ++ [instr]}
  return (code1 ++ code2 ++ [instr])

transUnOp :: UnOp -> Codegen UnOp_I
transUnOp (NOT_) = pure IR_NOT

transBinOp :: BinOp -> Codegen BinOp_I
transBinOp (Add) = pure IR_Add
transBinOp (Mul) = pure IR_Mul
