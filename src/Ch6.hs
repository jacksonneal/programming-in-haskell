module Ch6 where


fac :: Int -> Int
fac n = product [1 .. n]

fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac' (n - 1)

product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (x : xs) = evens xs

init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init xs

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

exp' :: Int -> Int -> Int
_ `exp'` 0 = 1
x `exp'` e = x * (x ^ (e - 1))

euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x < y = euclid x (y - x)
  | otherwise = euclid (x - y) x

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

select' :: [a] -> Int -> a
select' (a : _) 0 = a
select' (_ : xs) n = select' xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) = x == y || elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve l = splitAt n l
  where
    n = length l `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort l) (msort r)
  where
    (l, r) = halve xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

take' :: [a] -> Int -> [a]
take' _ 0 = []
take' [] _ = []
take' (x : xs) n = x : take' xs (n - 1)

last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last' xs
