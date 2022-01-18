{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module Suslik.AST
  where

-- import           Bracket

import           Data.List

type Name = String

data Predicate =
  MkPredicate
  { predicateName :: Name
  , predicateArgs :: [VarDecl]
  , predicateBranches :: [PredicateBranch]
  }

data SuslikType = IntType | SetType | LocType

data VarDecl =
  MkVarDecl
  { varDeclType :: SuslikType
  , varDeclVar  :: Name
  }

data ArgListDecl

data PredicateBranch =
  MkPredicateBranch
  { predicateBranchCond :: BasicProp
  , predicateBranchBody :: Prop
  }

data Prop =
  MkProp
  { propBasic :: BasicProp
  , propSep :: SepProp
  }

data BasicProp
  = BoolLit Bool
  | And BasicProp BasicProp
  | Or  BasicProp BasicProp
  | Not BasicProp
  | PropVar Name
  | Equal Expr Expr
  | Le Expr Expr

data SepProp =
  MkSepProp
  { sepPropClauses :: [SepPropClause]
  }

data SepPropClause
  = LocSize Expr Int
  | PointsTo Expr Expr
  | PredApply Name [Expr]

data Expr
  = Lit Int
  | ExprVar Name
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | IfThenElse BasicProp Expr Expr
  | SetSingleton Expr
  | SetUnion Expr Expr
  | SetEmpty
  | Apply Name [Expr]

data FunDecl =
  MkFunDecl
  { funDeclName :: Name
  , funDeclPre :: Prop
  , funDeclPost :: Prop
  }

class Emit a where
  emit :: a -> String

  -- emitString = getBracketed . emit
  -- emit = topLevelString . emitString

  default emit :: ParenEmit a => a -> String
  emit = parenEmit NoParens

-- defaultEmit :: Emit a => a -> Bracketed
-- defaultEmit = topLevelString . emitString

data ParenState = NoParens | WithParens

parens :: ParenState -> String -> String
parens NoParens = id
parens WithParens = ('(':) . (++")")

class {- Emit a => -} ParenEmit a where
  parenEmit :: ParenState -> a -> String

emitWithParens :: ParenEmit a => a -> String
emitWithParens = parenEmit WithParens

binOp :: ParenEmit a => ParenState -> String -> ParenState -> a -> a -> String
binOp opParens op ps x y = parens ps $ parenEmit opParens x ++ " " ++ op ++ " " ++ parenEmit opParens y

binOp' :: ParenEmit a => String -> ParenState -> a -> a -> String
binOp' = binOp WithParens

-- instance Emit Predicate where
--   emit p =
--     topLevel $ bracketed
--       (bLine ("predicate " <> predicateName p <> emitString (predicateArgs p))
--          : _ (emit undefined))

instance Emit [VarDecl] where
  emit [] = "()"
  emit xs = "(" <> argList <> ")"
    where
      argList = intercalate ", " $ map emit xs

instance Emit VarDecl where
  emit (MkVarDecl ty var) = emit ty <> " " <> emit var

instance Emit SuslikType where
  emit IntType = "int"
  emit SetType = "set"
  emit LocType = "loc"

instance Emit Name where
  emit = id

instance ParenEmit Expr where
  parenEmit ps = \case
    Lit i -> show i
    ExprVar v -> v
    Add x y -> binOp' "+" ps x y
    Sub x y -> binOp' "-" ps x y
    Mul x y -> binOp' "*" ps x y
    IfThenElse c t f -> parens ps $ emitWithParens c ++ " ? " ++ emitWithParens t ++ " : " ++ emitWithParens f
    SetSingleton x -> "{" ++ emit x ++ "}"
    SetUnion x y -> binOp' "++" ps x y
    SetEmpty -> "{}"
    Apply f xs -> f ++ "(" ++ intercalate ", " (map emit xs) ++ ")"

instance Emit Expr

instance ParenEmit BasicProp where
  parenEmit ps = \case
    BoolLit True -> "true"
    BoolLit False -> "false"
    And x y -> binOp' "&&" ps x y
    Or  x y -> binOp' "||" ps x y
    Not x   -> "not " ++ emitWithParens x
    PropVar v -> v
    Equal x y -> binOp' "==" ps x y
    Le x y -> binOp' "<=" ps x y

instance Emit BasicProp

instance Emit SepProp where
  emit (MkSepProp []) = "emp"
  emit (MkSepProp xs) = intercalate " ** " (map emit xs)

instance Emit SepPropClause where
  emit = \case
    LocSize loc sz -> "[" ++ emit loc ++ ", " ++ show sz ++ "]"
    PointsTo loc x -> parenEmit WithParens loc ++ " :-> " ++ emit x
    PredApply f xs -> emit (Apply f xs)

instance Emit Prop where
  emit p = "{ " ++ emit (propBasic p) ++ " ; " ++ emit (propSep p) ++ " }"

instance Emit PredicateBranch where
  emit b = "| " ++ emit (predicateBranchCond b) ++ " => " ++ emit (predicateBranchBody b)

instance Emit Predicate where
  emit p =
    unlines
      (("predicate " ++ predicateName p ++ emit (predicateArgs p) ++ " {")
        : map emit (predicateBranches p)
        ++ ["}"])

intDecl, setDecl, locDecl :: Name -> VarDecl
intDecl = MkVarDecl IntType
setDecl = MkVarDecl SetType
locDecl = MkVarDecl LocType

example :: Predicate
example =
  MkPredicate
  { predicateName = "lseg"
  , predicateArgs = [locDecl "x", setDecl "s"]
  , predicateBranches =
      [ MkPredicateBranch
        { predicateBranchCond = Equal (ExprVar "x") (ExprVar "null")
        , predicateBranchBody =
            MkProp
            { propBasic = Equal (ExprVar "s") SetEmpty
            , propSep =
                MkSepProp [ ]
            }
        }

      , MkPredicateBranch
        { predicateBranchCond = Not $ Equal (ExprVar "x") (ExprVar "null")
        , predicateBranchBody =
            MkProp
            { propBasic = Equal (ExprVar "s") (SetUnion (SetSingleton (ExprVar "v")) (ExprVar "s1"))
            , propSep =
                MkSepProp
                  [ LocSize (ExprVar "x") 2
                  , PointsTo (ExprVar "x") (ExprVar "v")
                  , PointsTo (Add (ExprVar "x") (Lit 1)) (ExprVar "nxt")
                  , PredApply "lseg" [ExprVar "nxt", ExprVar "s1"]
                  ]
            }
        }
      ]
  }

