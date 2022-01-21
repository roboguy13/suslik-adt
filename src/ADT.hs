{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ADT where

import           Suslik.AST
import           ERep

import           Data.Void
import           Data.Proxy

import           Data.List.NonEmpty (NonEmpty (..), cons)
import qualified Data.List.NonEmpty as NE
import           Data.Foldable

import           GHC.TypeLits

import           Data.Maybe (catMaybes)

import           GHC.Generics

type (.+.) = Either
type (.*.) = (,)

data Set

type family BaseType t where
  BaseType Int = Int
  BaseType Set = [Int]

type family ToSuslikType t where
  ToSuslikType Int = 'IntType
  ToSuslikType Set = 'SetType

data ADT t
  = MkADT
    { adtName :: Name
    , adtContents :: AdtContents t
    }

data NamedADT (name :: Symbol)

data AdtContents t where
  InL :: forall a b. AdtContents a -> AdtContents (a .+. b)
  InR :: forall a b. AdtContents b -> AdtContents (a .+. b)

  Pair :: forall a b. AdtContents a -> AdtContents b -> AdtContents (a .*. b)

  WrapADT :: forall a name. ADT a -> AdtContents (NamedADT name)

  Base :: SuslikSing (ToSuslikType a) => BaseType a -> AdtContents a
  -- FromList :: [Int] -> ADT Set

type SOP = NonEmpty ProductBranch

data ProductEntry = NamedEntry Name | UnnamedEntry SuslikType

type ProductBranch = NonEmpty ProductEntry

-- class {- ToSOP t => -} ToADT t where
--   type AdtRep t

--   toADT :: t -> ADT (AdtRep t)

-- data SOP where
--   SopOne  :: ProductBranch -> SOP
--   SopCons :: ProductBranch -> SOP -> SOP

-- data ProductBranch where
--   ProductOne  :: SuslikType -> ProductBranch
--   ProductCons :: SuslikType -> ProductBranch -> ProductBranch

-- toADT :: ERep t => t -> ADT t

-- class ToADT t where
--   toADT :: t -> ADT t

class ToSOP t where
  toSOP :: SOP

class ToProductBranch t where
  toProductBranch :: ProductBranch

class ToProductEntry t where
  toProductEntry :: ProductEntry

instance KnownSymbol name => ToProductEntry (NamedADT name) where
  toProductEntry = NamedEntry (symbolVal (Proxy @name))

instance (ToProductBranch a, ToSOP b) => ToSOP (a .+. b) where
  toSOP = cons (toProductBranch @a) (toSOP @b)

instance ToSOP Int where
  toSOP = (toProductBranch @Int) :| []

instance ToSOP Set where
  toSOP = (toProductBranch @Set) :| []

instance ToProductBranch Int where
  toProductBranch = UnnamedEntry IntType :| []

instance ToProductBranch Set where
  toProductBranch = UnnamedEntry SetType :| []

instance (ToProductEntry a, ToProductBranch b) => ToProductBranch (a .*. b) where
  toProductBranch = cons (toProductEntry @a) (toProductBranch @b)

-- sopGetNames :: SOP -> [Name]

sopToPredicate :: Name -> SOP -> Predicate
sopToPredicate name xs =
  MkPredicate
  { predicateName = name
  , predicateArgs = [locDecl "x"]
  , predicateBranches = toList $ fmap productToPredBranch xs
  }

productToPredBranch :: ProductBranch -> PredicateBranch
productToPredBranch branch =
  MkPredicateBranch
  { predicateBranchCond = BoolLit True
  , predicateBranchBody =
      MkProp
      { propBasic = BoolLit True
      , propSep = MkSepProp $ catMaybes $ toList $ fmap (uncurry go) (NE.zip locs branch)
      }
  }
  where
    go :: Expr -> ProductEntry -> Maybe SepPropClause
    go loc (NamedEntry name) = Just $ PredApply name [loc]
    go loc (UnnamedEntry x) = Nothing

    locs = NE.fromList $ ExprVar "x" : map toLoc [1..]
    toLoc ix = Add (ExprVar "x") (Lit ix)


