module BinTree where

import Data.Tree
import qualified Data.Foldable as F

data BinTree a = Empty | BinTree a (BinTree a) (BinTree a)

instance (Show a) => Show (BinTree a) where
    show (Empty) = ""
    show (BinTree a x y) = show (a)  ++ " (" ++ show (x) ++ ") (" ++ show (y) ++ ")"

instance (Eq a) => Eq (BinTree a) where
    BinTree a b c == BinTree x y z = a == x && b == y && c == z

instance Functor BinTree where
    fmap f (Empty) = Empty
    fmap f (BinTree a x y) = BinTree (f a) (fmap f x) (fmap f y)

instance Applicative BinTree where
    pure a = BinTree a Empty Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    BinTree func funcL funcR <*> BinTree inf infL infR = BinTree (func $ inf) (funcL <*> infL) (funcR <*> infR)

instance Monad BinTree where
    Empty >>= _ = Empty
    BinTree a b c >>= f = BinTree a' (b >>= f) (c >>= f)
        where BinTree a' b' c' = f a
    return a = BinTree a Empty Empty

instance Foldable BinTree where
    foldMap f (Empty) = mempty
    foldMap f (BinTree a b c) = f a `mappend` foldMap f b `mappend` foldMap f c

instance Traversable BinTree where
    traverse f Empty = pure Empty
    traverse f (BinTree a b c) = BinTree <$> (f a) <*> traverse f b <*> traverse f c

-- !!!
--instance Applicative BinTree 
--instance Foldable BinTree where
--instance Traversable BinTree where
--instance Monad BinTree where
-- !!!

--вычисление глубины дерева
depth :: BinTree a -> Int
depth (Empty) = 0
depth (BinTree a x y) = 1 + if depth (x) > depth (y) then depth (x) else depth (y)

--меняет местами два элемента в узле дерева
reverse' :: BinTree a -> BinTree a
reverse' (Empty) = Empty
reverse' (BinTree a x y) = BinTree a y x
 
--переводит бинарное дерево в список, возвращая все элементы в листьях дерева
leaves :: BinTree a -> [a]
leaves (Empty) = []
leaves (BinTree a x y) = a : leaves x ++ leaves y
