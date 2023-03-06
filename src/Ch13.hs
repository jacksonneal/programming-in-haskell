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
import System.IO

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

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
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

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural
      )
  symbol "]"
  return (n : ns)

eol :: Char
eol = '\n'

comment :: Parser ()
comment = do
  symbol "--"
  many (sat (/= eol))
  return ()

data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Expo Expr Expr
  deriving (Show)

expr :: Parser Expr
expr =
  do
    t <- term
    do
      symbol "+"
      Add t <$> expr
      <|> do
        symbol "-"
        Sub t <$> expr
      <|> return t

term :: Parser Expr
term =
  do
    f <- exponential
    do
      symbol "*"
      Mult f <$> term
      <|> do
        symbol "/"
        Div f <$> term
      <|> return f

exponential :: Parser Expr
exponential = do
  b <- factor
  do
    symbol "^"
    Expo b <$> exponential
    <|> return b

factor :: Parser Expr
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> fmap Val integer

evalA :: String -> Expr
evalA xs = case parse expr xs of
  [(n, [])] -> n
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "Invalid input"

box :: [String]
box =
  [ "+---------------+",
    "|               |",
    "+---+---+---+---+",
    "| q | c | d | = |",
    "+---+---+---+---+",
    "| 1 | 2 | 3 | + |",
    "+---+---+---+---+",
    "| 4 | 5 | 6 | - |",
    "+---+---+---+---+",
    "| 7 | 8 | 9 | * |",
    "+---+---+---+---+",
    "| 0 | ( | ) | / |",
    "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1 ..] box]

display :: [Char] -> IO ()
display xs = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (reverse (take 13 (reverse xs)))

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if c `elem` buttons
    then process c xs
    else do
      beep
      calc xs

process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC" = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n" = eval xs
  | c `elem` "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, [])] -> calc (show n)
  _ ->
    do
      beep
      calc xs

beep :: IO ()
beep = putStr "\BEL"

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

cls :: IO ()
cls = putStr "\ESC[2J"

run :: IO ()
run = do
  cls
  showbox
  clear
