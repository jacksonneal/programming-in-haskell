module Ch10 where

import Data.Char
import System.IO

main :: IO ()
main = do
  -- hangman
  -- nim
  life glider

-- * hangman

hangman :: IO ()
hangman = do
  putStrLn "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!!!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

-- * nim

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

playNim :: Board -> Int -> IO ()
playNim board player = do
  hSetBuffering stdin NoBuffering
  newline
  putBoard board
  if finished board
    then do
      newline
      putStr "Player "
      putStr (show (next player))
      putStrLn " wins!!!"
    else do
      newline
      putStr "Player "
      print player
      row <- getDigit "Enter a row number: "
      num <- getDigit "Star to remove: "
      if valid board row num
        then playNim (move board row num) (next player)
        else do
          putStrLn "ERROR: Invalid move"
          playNim board player

nim :: IO ()
nim = playNim initial 1

-- * life

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type LifeBoard = [Pos]

glider :: LifeBoard
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: LifeBoard -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: LifeBoard -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: LifeBoard -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) =
  ( ((x - 1) `mod` width) + 1,
    ((y - 1) `mod` height) + 1
  )

liveneighbs :: LifeBoard -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: LifeBoard -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

births :: LifeBoard -> [Pos]
births b =
  [ p | p <- rmdups (concatMap neighbs b), isEmpty b p, liveneighbs b p == 3
  ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen :: LifeBoard -> LifeBoard
nextgen b = survivors b ++ births b

life :: LifeBoard -> IO ()
life b = do
  cls
  showcells b
  wait 500000
  life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

-- * exercises

putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

putBoard' :: Board -> IO ()
putBoard' = putBoardInner 1
  where
    putBoardInner :: Int -> Board -> IO ()
    putBoardInner r [] = return ()
    putBoardInner r (x : xs) = do
      putRow r x
      putBoardInner (r + 1) xs

putBoardSeq :: Board -> IO ()
putBoardSeq xs = sequence_ [putRow r x | x <- xs, r <- [1 ..]]

adder :: IO ()
adder = do
  hSetBuffering stdin NoBuffering
  newline
  n <- getDigit "How many numbers? "
  xs <- sequence [getDigit "" | _ <- [1 .. n]]
  putStrLn ("The total is " ++ show (sum xs))
  newline

readLine' :: IO String
readLine' = do
  handle ""
  where
    handle :: String -> IO String
    handle xs = do
      x <- getCh
      case x of
        '\n' -> do
          putChar '\n'
          return xs
        '\DEL' -> do
          if null xs
            then handle ""
            else do
              putStr "\b \b"
              handle (init xs)
        _ -> do
          putChar x
          handle (xs ++ [x])
