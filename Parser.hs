module Parser (
  --parseExpr
) where

import Lexer
import Syntax
import Type

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
infixOp s f assoc = Ex.Infix (reservedOp s >> return f) assoc

-- Unary (prefix, postfix) and binary (infix) operators.
-- The list is ordered in descending order of precedence: that is,
-- all the operators in an individual list have the same
-- precedence, but may different associativity.
opTable :: Ex.OperatorTable String () Identity Expr
opTable = [
    [ prefixOp "!"   (UnApp NOT_)
    , prefixOp "-"   (UnApp SignInv)
    , prefixOp "len" (UnApp Len)
    , prefixOp "ord" (UnApp Ord)
    , prefixOp "chr" (UnApp Chr)
    ],
    [ infixOp "*" (BinApp Mul) Ex.AssocLeft
    , infixOp "/" (BinApp Div) Ex.AssocLeft
    , infixOp "%" (BinApp Mod) Ex.AssocLeft
    ],
    [ infixOp "+" (BinApp Add) Ex.AssocLeft
    , infixOp "-" (BinApp Sub) Ex.AssocLeft
    ],
    [ infixOp ">"  (BinApp GT_) Ex.AssocNone
    , infixOp ">=" (BinApp GTE_) Ex.AssocNone
    , infixOp "<"  (BinApp LT_) Ex.AssocNone
    , infixOp "<=" (BinApp LTE_) Ex.AssocNone
    ],
    [ infixOp "==" (BinApp EQ_) Ex.AssocLeft
    , infixOp "!=" (BinApp NEQ_) Ex.AssocLeft
    ],
    [ infixOp "&&" (BinApp AND_) Ex.AssocLeft],
    [ infixOp "||" (BinApp OR_) Ex.AssocLeft]
  ]

-------------------------------------------------------------------------
----------------------------EXPRESSION PARSERS---------------------------

intSign_P :: Parser (Int -> Int)
intSign_P =
  (char '+' >> return id) <|>
  (char '-' >> return negate) <|> return id

intLiteral_P :: Parser Expr
intLiteral_P = do
  sign <- intSign_P
  ds <- natural
  return (LInt (sign (fromInteger ds)))

boolLiteral_P :: Parser Expr
boolLiteral_P =
  (reserved "true" >> return (LBool True)) <|>
  (reserved "false" >> return (LBool False))

charLiteral_P :: Parser Expr
charLiteral_P = between (char '\'') (char '\'') character_P

character_P :: Parser Expr
character_P = do
  lchar <- (asciichar_P <|> escapedchar_P)
  return (LChar lchar)

asciichar_P :: Parser LitChar
asciichar_P = do
  c <- noneOf ['\\','\'', '\"']
  return (CustomChar c)

escapedchar_P :: Parser LitChar
escapedchar_P = do
  char '\\'
  c <- oneOf ['0','b','t','n','f','r','\"','\'','\\']
  return (EscapeSeq c)

stringLiteral_P :: Parser Expr
stringLiteral_P = between (char '\"') (char '\"') stringCharacters_P

stringCharacters_P :: Parser Expr
stringCharacters_P = do
  lcs <- many (asciichar_P <|> escapedchar_P)
  return (LString lcs)

pairLiteral_P :: Parser Expr
pairLiteral_P = do
  reserved "null"
  return (LPair "null")

ident_P :: Parser Expr
ident_P = do
  s <- identifier
  return (Ident s)

arrayelemexpr_P :: Parser Expr
arrayelemexpr_P = do
  arrelem <- arrayelem_P
  return (ArrayElemExpr arrelem)

arrayelem_P :: Parser ArrayElem
arrayelem_P = do
  i <- identifier
  es <- many1 (between (symbol "[") (symbol "]") expr_P)
  return (ArrayElem i es)


-- In order to create our expression parser, we combine expressions using the
-- operators defined in the expression syntax, together with the 'factor'
-- combinator below.
expr_P :: Parser Expr
expr_P = Ex.buildExpressionParser opTable expr_factor_P

-- At the bottom level, expressions are combined using the binary
-- and unary operators specified in the expression syntax. This is
-- why we refer to this function as 'expr_factor_P'.
expr_factor_P :: Parser Expr
expr_factor_P = try intLiteral_P
  <|> try boolLiteral_P
  <|> try charLiteral_P
  <|> try stringLiteral_P
  <|> try pairLiteral_P
  <|> try ident_P
  <|> try arrayelemexpr_P
  <|> parens expr_P

------------------------------------------------------------------------------
-------------------------------STATEMENT PARSERS------------------------------

-- In 'try statTopLevel_P', lazy evaluation should
-- ensure that we do not get excessively deep trees (i.e: call stacks):
-- since each monadic 'run' operation (i.e: each statement 'a <- xa' in
-- a 'do' block) is atomic, there should only be one of the other
-- parsers below running in memory at any given time.
stat_P :: Parser Stat
stat_P = try skip_P
  <|> try vardef_P
  <|> try varref_P
  <|> try readStat_P
  <|> try readStat_P
  <|> try freeStat_P
  <|> try returnStat_P
  <|> try exitStat_P
  <|> try printStat_P
  <|> try printlnStat_P
  <|> try ifStat_P
  <|> try whileStat_P
  <|> beginEndStat_P

-- TODO: Figure out how to parse recursive statements according to
-- grammar in spec.
-- Or perhaps we can specify a program as containing a list of statements
-- rather than one statement and after creating the AST, we can use a
-- fold to combine them into a single statement?
statTopLevel_P :: Parser [Stat]
statTopLevel_P = sepBy1 stat_P (lexeme (char ';'))

skip_P :: Parser Stat
skip_P = reserved "skip" >> return Skip

vardef_P :: Parser Stat
vardef_P = do
  t <- lexeme type_P
  i <- identifier
  reserved "="
  rhs <- lexeme assignRHS_P
  return (VarDef t i rhs)

varref_P :: Parser Stat
varref_P = do
  lhs <- lexeme assignLHS_P
  reserved "="
  rhs <- lexeme assignRHS_P
  return (VarRef lhs rhs)

readStat_P :: Parser Stat
readStat_P = do
  reserved "read"
  lhs <- lexeme assignLHS_P
  return (ReadStat lhs)

freeStat_P :: Parser Stat
freeStat_P = do
  reserved "free"
  e <- lexeme expr_P
  return (FreeStat e)

returnStat_P :: Parser Stat
returnStat_P = do
  reserved "return"
  e <- lexeme expr_P
  return (Return e)

exitStat_P :: Parser Stat
exitStat_P = do
  reserved "exit"
  e <- lexeme expr_P
  return (Exit e)

printStat_P :: Parser Stat
printStat_P = do
  reserved "print"
  e <- lexeme expr_P
  return (Print e)

printlnStat_P :: Parser Stat
printlnStat_P = do
  reserved "println"
  e <- lexeme expr_P
  return (PrintLn e)

ifStat_P :: Parser Stat
ifStat_P = do
  reserved "if"
  cond <- lexeme expr_P
  reserved "then"
  st1 <- lexeme stat_P
  reserved "else"
  st2 <- lexeme stat_P
  reserved "fi"
  return (If cond st1 st2)

whileStat_P :: Parser Stat
whileStat_P = do
  reserved "while"
  e <- lexeme expr_P
  reserved "do"
  st <- lexeme stat_P
  reserved "done"
  return (While e st)

beginEndStat_P :: Parser Stat
beginEndStat_P = do
  reserved "begin"
  st <- lexeme stat_P
  reserved "end"
  return (BeginEnd st)

------------------------------------------------------------------------------

assignLHS_P :: Parser AssignL
assignLHS_P = try (identifier >>= return . AssignLIdent)
  <|> try (arrayelem_P >>= return . AssignLArrayElem)
  <|> (pairelem_P >>= return . AssignLPairElem)

assignRHS_P :: Parser AssignR
assignRHS_P = try (expr_P >>= return . AssignRExpr)
  <|> try (brackets (commaSep expr_P) >>= return . AssignRLitArray)
  <|> try newpair_P
  <|> try (pairelem_P >>= return . AssignRPairElem)
  <|> funcall_P
  where
    newpair_P = do
      reserved "newpair"
      (e1, e2) <- parens tupleExpr_P
      return (AssignRNewPair e1 e2)
      where
        tupleExpr_P = do
          a1 <- expr_P
          char ','
          a2 <- expr_P
          return (a1, a2)
    funcall_P = do
      reserved "call"
      i <- identifier
      as <- parens (commaSep expr_P)
      return (AssignRFunCall i as)

pairelem_P :: Parser PairElem
pairelem_P
  = try (do {(reserved "fst"); e <- expr_P; return (Fst e)})
  <|> (do {(reserved "snd"); e <- expr_P; return (Snd e)})

------------------------------------------------------------------------------
-------------------------------TYPE PARSERS-----------------------------------

type_P :: Parser Type
type_P = try tyArr_P
  <|> tyPair_P
  <|> util_baseType_P

tyInt_P :: Parser Type
tyInt_P = do
  reserved tyNameInt
  return tyInt

tyBool_P :: Parser Type
tyBool_P = do
  reserved tyNameBool
  return tyBool

tyChar_P :: Parser Type
tyChar_P = do
  reserved tyNameChar
  return tyChar

tyString_P :: Parser Type
tyString_P = do
  reserved tyNameString
  return tyString

tyArr_P :: Parser Type
tyArr_P = do
  t <- util_baseType_P
  syms <- many1 (symbol "[]")
  let n = length syms
  -- This is used in order to handle multi-dimensional arrays
  let t' = foldr TApp t (replicate (n-1) (TCon "array"))
  return (tyArr t')

tyPair_P :: Parser Type
tyPair_P = do
  reserved tyNamePair
  (t1, t2) <- parens internalPair_P
  return (tyPair t1 t2)
  where
    -- Used to parse the internals of the pair
    internalPair_P = do
      t1 <- lexeme pairelemtype_P
      lexeme (char ',')
      t2 <- lexeme pairelemtype_P
      return (t1, t2)
      where
        pairelemtype_P =
          (try tyArr_P)         <|>
          (try util_baseType_P) <|>
          (try (reserved tyNamePair >> return (TCon tyNamePair)))

-- We have a special utility function for parsing base types
-- Since base types are "non-recursive" types, i.e: types of kind '*',
-- all types are built from base types with various type constructors.
-- This function abstracts away the need to call long base type parsers
-- in higher-kinded types.
util_baseType_P :: Parser Type
util_baseType_P
   =  tyInt_P
  <|> tyBool_P
  <|> tyChar_P
  <|> tyString_P
-------------------------------------------------------------------------------
-----------------TOP LEVEL PARSERS AND THEIR UTILITY PARSERS-------------

program_P :: Parser Program
program_P = do
  reserved "begin"
  fs <- lexeme (try (many function_P))
  st <- lexeme stat_P
  reserved "end"
  return (Program fs st)

function_P :: Parser Function
function_P = do
  t <- type_P
  i <- identifier
  params <- parens (commaSep param_P)
  reserved "is"
  st <- stat_P
  reserved "end"
  return (Function t i params st)

param_P :: Parser Param
param_P = do
  t <- type_P
  i <- identifier
  return (Param t i)

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  res <- p
  eof
  return res

parseTopLevelStat_P :: String -> Either ParseError [Stat]
parseTopLevelStat_P s = parse (contents statTopLevel_P) "<stdin>" s

parseTopLevelProgram_P :: String -> Either ParseError Program
parseTopLevelProgram_P s = parse (contents program_P) "<stdin>" s

------------------------------------------------------------------
