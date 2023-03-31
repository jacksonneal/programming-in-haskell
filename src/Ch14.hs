module Ch14 where

import Data.Foldable

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   -- mempty :: (a, b)
--   mempty = (mempty, mempty)
--
--   -- mappend :: (a, b) -> (a, b) -> (a,b)
--   (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- instance Monoid b => Monoid (a -> b) where
--   -- mempty :: (a -> b)
--   mempty = const mempty
--
--   -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--   fl `mappend` fr = \x -> fl x `mappend` fr x

-- instance Foldable Maybe where
--   -- fold :: Monoid a => Maybe a -> a
--   fold Nothing = mempty
--   fold (Just a) = a
--
--   -- foldMap :: Monoid a => (a -> b) -> Maybe a -> b
--   foldMap _ Nothing = mempty
--   foldMap f (Just a) = f a
--
--   -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
--   foldr _ _ Nothing = mempty
--   foldr f v (Just a) = f a v
--
--   -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
--   foldl _ _ Nothing = mempty
--   foldl f v (Just b) = f v b

-- instance Traversable Maybe where
--   -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
--   traverse g Nothing = pure Nothing
--   traverse g (Just a) = Just <$> g a

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)

instance Foldable Tree where
  -- fold :: Monoid a => Maybe a -> a
  fold Leaf = mempty
  fold (Node l v r) = fold l <> v <> fold r

  -- foldMap :: Monoid a => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f a Leaf = a
  foldr f a (Node l v r) = foldr f (foldr f (f v a) r) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f a Leaf = a
  foldl f a (Node l v r) = foldl f (foldl f (f a v) l) r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> if f x then [x] else mempty)
