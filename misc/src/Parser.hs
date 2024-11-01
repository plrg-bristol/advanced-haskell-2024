module Parser where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

type Parser = ReadP

parse :: Parser a -> String -> [(a, String)]
parse p str = readP_to_S p str

digit :: Parser Int
digit = fmap (read . pure) (satisfy isDigit)

int :: Parser Int
-- int :: ReadP [Char]
int = fmap read $ many1 (satisfy isDigit)
