-- Jakub Staroń, 2017

{-# LANGUAGE TypeOperators #-}

module RList where

import Data.Functor (Functor)
import Data.Foldable (Foldable, toList)
import Data.List (intercalate)
import FormatString (format)

data RList a =
    Nil
  | (RList a) :> a
  deriving (Eq)

instance Functor RList where
    fmap f (xs :> x) = fmap f xs :> f x
    fmap _ Nil = Nil

instance Monoid (RList a) where
    mempty = Nil
    mappend xs Nil = xs
    mappend xs (ys :> y) = xs `mappend` ys :> y
    mconcat = foldl mappend mempty

instance Foldable RList where
   foldMap _ Nil = mempty
   foldMap f (xs :> x) = foldMap f xs `mappend` f x
   foldr _ acc Nil = acc
   foldr f acc (xs :> x) = acc `seq` foldr f (f x acc) xs

instance (Show a) => Show (RList a) where
    show list = case list of
        Nil -> "[]"
        cons -> format "[%0]" [intercalate ", " (toList (fmap show cons))]
