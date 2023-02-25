{-# LANGUAGE LambdaCase #-}

module Ch13 where

{-
A parser for things
Is a function from string
To lists of pairs
Of things and strings
-}

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item =
  P
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )
