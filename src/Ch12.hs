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

newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z gs <*> Z xs = Z [g x | (g, x) <- zip gs xs]
