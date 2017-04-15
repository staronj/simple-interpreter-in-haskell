-- Jakub Staroń, 2017

{-# LANGUAGE TypeOperators #-}

module DiffList where

import Data.Functor
import Data.Foldable
import Data.List

data DiffList a =
    Nil
  | (DiffList a) :> a
  deriving (Eq)

instance Functor DiffList where
    fmap f (xs :> x) = (fmap f xs) :> (f x)
    fmap f Nil = Nil

instance Monoid (DiffList a) where
    mempty = Nil
    mappend xs Nil = xs
    mappend xs (ys :> y) = xs `mappend` ys :> y
    mconcat = foldl mappend mempty

instance Foldable DiffList where
   foldMap f Nil = mempty
   foldMap f (xs :> x) = foldMap f xs `mappend` f x
   foldr f acc Nil = acc
   foldr f acc (xs :> x) = acc `seq` foldr f (f x acc) xs

instance (Show a) => Show (DiffList a) where
    show list = case list of
        Nil -> "[]"
        cons -> "[" ++ intercalate ", " (toList (fmap show cons)) ++ "]"


foo :: Int -> DiffList Int
foo n = let ns = [1..n] in
    foldl' (:>) Nil ns

foo2 :: Int -> DiffList Int
foo2 n = let ns = [1..n] in
    foldr' (\n acc -> (Nil :> n) `mappend` acc) Nil ns
