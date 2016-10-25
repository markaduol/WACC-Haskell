--------------------------------------------------------------------------------
-- Module: Type

-- Purpose: Contains structure for how types will be represented in AST
--------------------------------------------------------------------------------

module Type where

{-'TVar Name' is used to represent type variables, i.e: variables that
  represent general types
-}
{-'TCon Name' is used to represent type constructors. Since different types
  can have different kinds, we only declare the type constructor and not the
  types that will be fed into the type constructor. Using type constructors
  rather than declaring explicit types for pairs and arrays makes our language
  more flexible, as well as improves our ability to perform type inference.
-}
{-'TApp Type Type' is used to represent the application of two types to form
  a new type. Since 'TApp Type Type' is defined recursively, we can use it to
  build types of any kind, as in *; * -> *; * -> * -> *; and so on. See below
  for examples
-}
type Name = String

data Type
  = TVar Name
  | TCon Name -- Consider whether to add field for arity
  | TApp Type Type
  deriving (Show, Eq)

-- It only makes sense to create a 'TApp t1 t2' if the first type is a type
-- constructor - of the form 'TCon t'
mkTApp :: Name -> [Type] -> Type
mkTApp tconName ts = foldl TApp (TCon tconName) ts

tyPair :: Type -> Type -> Type
tyPair (TApp (TApp (TCon "pair") _) _) _ = error "Cannot create nested pair types"
tyPair _ (TApp (TApp (TCon "pair") _) _) = error "Cannot create nested pair types"
tyPair t1 t2 = mkTApp tyNamePair [t1, t2]

tyArr :: Type -> Type
tyArr (TCon "array") = error "Cannot construct type: TApp ((TCon \"array\") (TCon \"array\"))"
tyArr t = mkTApp tyNameArr [t]

tyInt, tyBool, tyChar, tyString :: Type
tyInt    = TCon tyNameInt
tyBool   = TCon tyNameBool
tyChar   = TCon tyNameChar
tyString = TCon tyNameString

tyNamePair, tyNameArr, tyNameInt, tyNameBool, tyNameChar, tyNameString :: Name
tyNamePair = "pair"
tyNameArr = "array"
tyNameInt = "int"
tyNameBool = "bool"
tyNameChar = "char"
tyNameString = "string"

-- Left-recursive deconstruction of Type 'TApp t1 t2'. Since 'TApp t1 t2' is
-- built in a left-recursive fashion when using 'mkTApp', this function allows
-- us present 'TApp t1 t2' in a more user/human friendly format. In fact we can
-- derive the kind of the top level type constructor 't1', simply by taking
-- the length of list and subtracting 1.
-- N.B: This only works if we built 'TApp t1 t2' using 'mkTApp'
-- (which we should)
viewTyApp :: Type -> [Type]
viewTyApp t = deconstr t []
  where
    deconstr (TApp t1 t2) acc = deconstr t1 (t2:acc)
    deconstr t acc = t:acc
