module AST_Traversal where

import Control.Applicative
import Control.Monad
import Syntax

-- Descend monad: allows us to descend a recursive statement, down to its
-- terminal statements. We then apply some function 'f' to these terminal
-- statements.
-- Therefore, after invoking this function, we can freely assume that the
-- result is one of the non-recursive statements.
-- Notice the StatSeq case descends the right subtree followed by the left
-- subtree. This is because our parser sequences statements in a right
-- associative backwards, to forwards manner.
-- We must maintain the constructors in order to preserve the structure of
-- the tree.
descendStatM :: (Monad m) => (Stat -> m Stat) -> Stat -> m Stat
descendStatM f st = case st of
  StatSeq s1 s2 -> StatSeq <$> descendStatM f s2 <*> descendStatM f s1
  BeginEnd s    -> BeginEnd <$> descendStatM f s
  While e s     -> While e <$> descendStatM f s
  If e s1 s2    -> If e <$> descendStatM f s1 <*> descendStatM f s2
  otherwise     -> f st

descendExprM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
descendExprM f ex = case ex of
  BinApp op e1 e2 -> BinApp op <$> descendExprM f e1 <*> descendExprM f e2
  UnApp op e      -> UnApp op <$> descendExprM f e
  otherwise       -> f e
