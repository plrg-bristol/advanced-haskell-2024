{-# LANGUAGE InstanceSigs #-}
module Arrows where
import Control.Category (Category ((.)), id, (>>>))

newtype Kleisli a b = K (a -> IO b)

getInt :: IO Int
getInt = do
  line <- getLine
  pure (read line)

eg :: Int -> IO Int
eg x = do
  y <- getInt
  pure (x + y)

egPrint :: Int -> IO ()
egPrint x = print x

instance Category Kleisli where
  id :: Kleisli a a
  id = K pure
  (.) :: Kleisli b c -> Kleisli a b -> Kleisli a c
  K f . K g = K (\x -> g x >>= f)

final :: Kleisli Int ()
final = K eg >>> K egPrint

runK :: Kleisli a b -> a -> IO b
runK (K f) x = f x

egIO :: Int -> IO ()
egIO x = runK final x
