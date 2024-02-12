{-# LANGUAGE InstanceSigs #-}
module MoreMonads where

data Tree a = Fork (Tree a) (Tree a) | Leaf a deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Fork t1 t2) = Fork (fmap f t1) (fmap f t2)
  fmap f (Leaf x) = Leaf (f x)

instance Applicative Tree where
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  -- (Leaf f) <*> (Leaf x) = Leaf (f x)
  -- (Leaf f) <*> (Fork t1 t2) = Fork (fmap f t1) (fmap f t2)
  (Leaf f) <*> tx = fmap f tx
  (Fork tf1 tf2) <*> tx = Fork (tf1 <*> tx) (tf2 <*> tx) --  (Fork t1 t2)

  pure x = Leaf x

instance Monad Tree where
  return = pure

  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  Leaf x >>= f = f x
  Fork t1 t2 >>= f = Fork (t1 >>= f) (t2 >>= f)

join :: Monad m => m (m a) -> m a
join mmx = mmx >>= id
