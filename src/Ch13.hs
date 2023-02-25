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

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp ->
          case parse p inp of
            [] -> []
            [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
  where
    g x y z = (x, z)

three' :: Parser (Char, Char)
three' = do
  x <- item
  item
  z <- item
  return (x, z)
