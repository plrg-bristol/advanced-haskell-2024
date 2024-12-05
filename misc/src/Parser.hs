{-# LANGUAGE LambdaCase #-}

module Parser where

-- import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Arrow

-- type Parser = ReadP

-- parse :: Parser a -> String -> [(a, String)]
-- parse p str = readP_to_S p str

-- digit :: Parser Int
-- digit = fmap (read . pure) (satisfy isDigit)

-- int :: Parser Int
-- -- int :: ReadP [Char]
-- int = fmap read $ many1 (satisfy isDigit)

-- based on yoda - https://hackage.haskell.org/package/yoda

newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\case
  [] -> []
  (t:ts') -> [(t, ts')])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p)
    = Parser (\ts -> fmap (first f) (p ts))