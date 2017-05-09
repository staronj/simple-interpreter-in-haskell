-- Jakub Staroń, 2017

{-# LANGUAGE DeriveFunctor #-}

module RList where

import Data.Functor
import Data.Foldable
import Data.Monoid
import Data.List (intercalate)
import FormatString (format)

data RList a =
    Nil
  | (RList a) :> a
  deriving (Eq, Functor)

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
    show Nil = "[]"
    show cons = format "[%0]" [intercalate ", " $ toList $ fmap show cons]
