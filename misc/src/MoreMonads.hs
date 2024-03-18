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

newtype Reader a b = Reader { runReader :: a -> b }

newtype Reader' a b = Reader' (a -> b)
runReader' :: Reader' a b -> (a -> b)
runReader' (Reader' f) = f

eg1, eg2, eg3 :: Reader String Int
eg1 = Reader{runReader = length}
eg2 = Reader length
eg3 = Reader (\x -> 5)

instance Functor (Reader a) where
  fmap :: (b -> c) -> Reader a b -> Reader a c
  fmap f (Reader g) = Reader (f . g)


instance Applicative (Reader e) where
  pure :: a -> Reader e a
  pure x = Reader $ const x
  (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
  Reader f <*> Reader g = Reader $ \e -> f e (g e)

instance Monad (Reader e) where
  (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  Reader f >>= g = Reader $ \e -> runReader (g (f e)) e

egDo :: Reader a a
egDo = do
  env <- ask
  pure env 

egDo' :: a -> a
egDo' = \x -> x

ask :: Reader e e
ask = Reader $ \x -> x

-- silly bullshit you can do with the Functor instance for `(->) e`
-- because `fmap` is `(.)` for the unwrapped reader monad...
-- >>> fmap (+ 1) (* 10) 0
-- 1
-- >>> fmap (* 10) (+ 1) 0
-- 10
-- >>> (* 10) `fmap` (+ 1) $ 0
-- 10

-- two different fmaps!
-- >>> fmap (fmap (+ 1) (* 10)) [1..3]
-- [11,21,31]

-- honestly i don't even know what's going on here...
-- >>> fmap fmap fmap (+ 1) [[1,2],[3,4]]
-- [[2,3],[4,5]]

-- you can just keep adding fmaps to create pointless new horrors:
-- >>> fmap fmap fmap fmap fmap (+ 1) (* 10) [1..3]
-- [11,21,31]
-- >>> fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap (+) [0,1,2] <*> [[1,2,3]]
-- [[1,2,3],[2,3,4],[3,4,5]]
-- (again, please don't do this)
