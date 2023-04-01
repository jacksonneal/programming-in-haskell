module Ch15 where

primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

repeat' :: a -> Tree a
repeat' x = xs where xs = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' _ Leaf = Leaf
take' n (Node l x r) = Node (take' (n - 1) l) x (take' (n - 1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

sqroot :: Double -> Double
sqroot n = snd . head $ dropWhile pred (zip it (tail it))
  where
    approx = 0.00001
    pred (x, y) = abs (x - y) > approx
    it = iterate next n
    next a = (a + n / a) / 2
