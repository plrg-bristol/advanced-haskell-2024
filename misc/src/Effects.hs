{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Effects where

import Control.Monad

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
data Free f anything where
  Pure :: a -> Free f a
  -- Bind :: Free f a -> (a -> Free f b) -> Free f b
  Bind :: f a -> (a -> Free f b) -> Free f b
  -- Impure :: f a -> Free f a

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

instance Applicative (Free f) where
  -- a -> Free f a
  pure = Pure
  (<*>) = undefined -- ap

instance Functor f => Monad (Free f) where
  -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Pure x) >>= g = g x
  -- (Bind fa k) >>= g = fmap k fa
  (Bind fa k) >>= g = Bind fa (k >=> g)

handle :: Listy a -> [a]
handle = interp
  where
    interp :: Listy a -> [a]
    -- interp :: Free L a -> [a]
    interp (Pure x) = [x]
    -- interp (Pure x) = pure x
    interp (Bind la cont) = interpL la >>= (interp . cont)

    interpL :: L a -> [a]
    interpL = undefined

