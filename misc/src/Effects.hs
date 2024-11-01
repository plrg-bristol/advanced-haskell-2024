{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Effects where

-- effects
-- * exceptions - normal exceptions blow up, exceptions expressed as handlers can potentially resume
-- * formal tracking of impure

-- "handlers"

-- haskell funs "pure"
-- effects give controlled level of impurity

-- effects we know of:
-- IO
-- Either String a -- informative exceptions
-- Maybe - exceptions
-- Identity -- empty effect
newtype Identity a = Identity a
-- :i Monad
-- [a] -- non-det
-- State
-- Gen -- randomness
-- ST - state, but with IO API

-- Plan
-- Free Monad (DSL, syntatic monad)
-- (Freer, https://okmij.org/ftp/Computation/free-monad.html)
-- precursor of Alg effects

-- GADT
data Free f a where
  Pure :: a -> Free f a
  Impure :: f a -> (a -> Free f b) -> Free f b

-- ~
-- data Free f a
--   = Pure a
--   | Impure (f a) (a -> Free f b)

deriving instance Functor (Free f)

data L a where
  Empty :: L a
  Cons :: a -> L a -> L a
  deriving Functor

type Listy = Free L

-- instance Applicative Listy where
--   pure = return
--   (<*>) = ap

-- instance FMonad Listy where

