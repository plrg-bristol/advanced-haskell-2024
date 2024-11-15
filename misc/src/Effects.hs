{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Effects where

import Control.Monad
import Data.Char (toUpper)

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

instance Monad (Free f) where
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

data FileRead a where
  ReadFile :: FilePath -> FileRead String
  WriteFile :: FilePath -> String -> FileRead ()

egRead :: Free FileRead Int
egRead = Bind (ReadFile "README.md") (pure . length)

-- readFileE :: FilePath -> Free FileRead String
-- readFileE file = Bind (ReadFile file) Pure

-- writeFileE :: FilePath -> String -> Free FileRead ()
-- writeFileE file contents = Bind (WriteFile file contents) Pure

liftE :: f a -> Free f a
liftE fx = Bind fx Pure

egRead' :: Free FileRead Int
egRead' = liftE (ReadFile "README.md") >>= (pure . length)

egRead'' :: Free FileRead Int
egRead'' = do
  contents <- liftE (ReadFile "README.md")
  pure (length contents)

egRW :: Free FileRead ()
egRW = do
  contents <- liftE (ReadFile "README.md")
  let contents' = map toUpper contents

  liftE (WriteFile "README-SHOUTY.md" contents')

handleFileRead :: Free FileRead a -> IO a
handleFileRead (Pure x) = pure x
handleFileRead (Bind (ReadFile file) g) = do
  contents <- readFile file
  handleFileRead (g contents)
handleFileRead (Bind (WriteFile file contents) g) = do
  x <- writeFile file contents
  handleFileRead (g x)

runHandler :: Monad m => (forall b. f b -> m b) -> Free f a -> m a
runHandler handler (Pure x) = pure x
runHandler handler (Bind fx g) = do
  x <- handler fx
  runHandler handler (g x)

fileReadIOHandler :: FileRead a -> IO a
fileReadIOHandler = \case
  ReadFile file -> readFile file
  WriteFile file contents -> writeFile file contents

fileReadListHandler :: FileRead a -> [a]
fileReadListHandler = \case
  ReadFile file -> ["dummy contents"]
  WriteFile file contents -> [()]


-- Look at polysemy: https://www.youtube.com/watch?v=kIwd1D9m1gE
-- On efficiency of effects, Alexis King: https://www.youtube.com/watch?v=0jI-AlWEwYI
