{-# LANGUAGE FlexibleInstances #-}
module Suslik.AST
  where

import           Bracket

import           Data.List

type Name = String

data Predicate =
  MkPredicate
  { predicateName :: Name
  , predicateArgs :: [VarDecl]
  , predicateBranches :: PredicateBranch
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
  = BP_And BasicProp BasicProp
  | BP_Or  BasicProp BasicProp
  | BP_Not BasicProp
  | BP_Bool Bool
  | BP_Var Name
  | BP_Equal Expr Expr
  | BP_Le Expr Expr

data SepProp =
  MkSepProp
  { sepPropClauses :: [SepPropClause]
  }

data SepPropClause
  = LocSize Int
  | PointsTo Expr

data Expr
  = Lit Int
  | ExprVar Name
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | IfThenElse BasicProp Expr Expr
  | SetSingleton Expr
  | SetUnion Expr Expr

data FunDecl

class Emit a where
  emit :: a -> Bracketed
  emitString :: a -> String

  emitString = getBracketed . emit

instance Emit Predicate where
  emit p =
    topLevel $ bracketed
      [ bLine ("predicate " <> predicateName p <> emitString (predicateArgs p))
      ]

instance Emit [VarDecl] where
  emit [] = topLevelString "()"
  emit xs = topLevelString $ "(" <> argList <> ")"
    where
      argList = intercalate ", " $ map emitString xs

instance Emit VarDecl where
  emit (MkVarDecl ty var) = topLevelString $ emitString ty <> " " <> emitString var

instance Emit SuslikType where
  emit IntType = topLevelString "int"
  emit SetType = topLevelString "set"
  emit LocType = topLevelString "loc"

instance Emit Name where
  emit = topLevelString
  emitString = id

