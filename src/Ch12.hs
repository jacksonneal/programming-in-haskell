module Ch12 where

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- instance Functor ((->) a) where
--   -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)
--
-- instance Applicative ((->) a) where
--   -- pure :: b -> (a -> b)
--   pure = const
--
--   -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
--   g <*> h = \x -> g x $ h x
--
-- instance Monad ((->) a) where
--   -- return :: b -> (a -> b)
--   return = pure
--
--   -- (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
--   (>>=) f g r = g (f r) r

newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z gs <*> Z xs = Z [g x | (g, x) <- zip gs xs]

-- pure id <*> x = x
-- left
-- id :: a -> a
-- x :: f a
-- pure id :: f (a -> a)
-- pure id <*> x = f a
-- right
-- x :: f a
--
-- pure (g x) = pure g <*> pure x
-- left
-- g :: a -> b
-- x :: a
-- g x :: b
-- pure (g x) :: f b
-- right
-- pure g :: f (a -> b)
-- pure x :: f a
-- pure g <*> pure x :: f b
--
-- x <*> pure y = pure (\g -> g y) <*> x
-- left
-- x :: f (a -> b)
-- y :: a
-- pure y :: f a
-- x <*> pure y :: f b
-- right
-- g :: a -> b
-- g y :: b
-- (\g -> g y) :: (a -> b) -> b
-- pure (\g -> g y) :: f ((a -> b) -> b)
-- pure (\g -> g y) <*> x :: f b
--
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- left
-- x :: f (b -> c)
-- y :: f (a -> b)
-- z :: f (a)
-- y <*> z :: f b
-- x <*> (y <*> z) :: f c
-- right
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- pure (.) :: f ((b -> c) -> (a -> b) -> (a -> c))
-- pure (.) <*> x :: f ((a -> b) -> (a -> c))
-- pure (.) <*> x <*> y :: f (a -> c)
-- (pure (.) <*> x <*> y) <*> z :: f c
