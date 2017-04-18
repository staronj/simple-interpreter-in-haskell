-- Jakub Staroń, 2017

{-# LANGUAGE TypeOperators #-}

module RList where

import Data.Functor
import Data.Foldable
import Data.List

data RList a =
    Nil
  | (RList a) :> a
  deriving (Eq)

instance Functor RList where
    fmap f (xs :> x) = (fmap f xs) :> (f x)
    fmap f Nil = Nil

instance Monoid (RList a) where
    mempty = Nil
    mappend xs Nil = xs
    mappend xs (ys :> y) = xs `mappend` ys :> y
    mconcat = foldl mappend mempty

instance Foldable RList where
   foldMap f Nil = mempty
   foldMap f (xs :> x) = foldMap f xs `mappend` f x
   foldr f acc Nil = acc
   foldr f acc (xs :> x) = acc `seq` foldr f (f x acc) xs

instance (Show a) => Show (RList a) where
    show list = case list of
        Nil -> "[]"
        cons -> "[" ++ intercalate ", " (toList (fmap show cons)) ++ "]"
