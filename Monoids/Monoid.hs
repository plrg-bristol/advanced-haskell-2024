{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monoid where

-- Semigroup

-- <Set (Type), op>

-- Int, +

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (+)

-- Int, *

newtype Pint = Pint Int deriving (Num, Show)

instance Semigroup Pint where
  (<>) :: Pint -> Pint -> Pint
  (<>) = (*)

-- Monoid
-- <Set (Type), op, id>

-- Int, +

instance Monoid Int where
  mempty = 0

-- Int, *

instance Monoid Pint where
  mempty = 1

-- other monoids (pre-defined in base, similarly to Pint): https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Monoid.html#g:2

-- Kiran:
-- a semigroup walks into a monoid bar, and they ask him for his ID

-- monads are monoids on the cat of endofunctors

-- monads
-- bi op - join
-- id - pure

-- bind :: m a -> (a -> m b) -> m b
-- pure / return a -> m a
-- join :: m (m a) -> m a

-- student recommended post: https://chrispenner.ca/posts/wc


