{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module ADT where

type (:+:) = Either
type (:*:) = (,)

data ADT t where
  InL :: forall a b. a -> ADT (a :+: b)
  InR :: forall a b. b -> ADT (a :+: b)

  Pair :: forall a b. a -> b -> ADT (a :*: b)

  Base :: Int -> ADT Int

class SuslikPred ty where

