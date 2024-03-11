{-# language GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
module GADTs where

import Prelude hiding (Maybe(..))

data Maybe a where
  Nothing :: Maybe Int
  Just :: a -> Maybe a

deriving instance Eq a => Eq (Maybe a)

data Maybe' a = Nothing' | Just' a

eg1, eg2 :: Maybe Int
eg1 = Nothing
eg2 = Just 4

eg3 :: Maybe String
eg3 = Nothing

maybe :: a -> Maybe a -> a
maybe x (Just y) = y
maybe x Nothing = 0 -- _

maybe' :: a -> Maybe a -> a
maybe' x (Just y) = y
maybe' x m
  | m == Nothing = _

data Nat = Z | S Nat

three :: Nat
three = S (S (S Z))

data Vec (n :: Nat) a where
  Empty :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

emptyList :: Vec Z Int
emptyList = Empty
-- emptyList = Cons 1 Empty

twoList :: Vec (S (S Z)) Int
twoList = Cons 1 (Cons 2 Empty)
-- twoList = Empty

data SNat (n :: Nat) where
  Z' :: SNat Z
  S' :: SNat n -> SNat (S n)

-- index :: LessThan m n => Vec n a -> SNat m -> a