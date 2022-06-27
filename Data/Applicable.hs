{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module: Data.Applicable
Description: The 'Applicable' class
Copyright: ⓒ 2022 Anselm Schüler
License: MIT
Maintainer: mail@anselmschueler.com

The 'Applicable' class with its operator '($*)'.
You will likely need the @FlexibleContexts@ extension to use this module’s instances.
-}

module Data.Applicable (
  Applicable(..),
  ApplyTo(..),
  ApplyMap(..),
  ApplyAp(..),
  ApplyBind(..),
  GroupAction(..),
  ChurchBool(..),
  ChurchNumeral(..),
  ChurchTuple(..)
) where

import Data.List (genericIndex)
import Data.Bifunctor (Bifunctor)
import Data.Ix
import Data.Data
import GHC.Generics

-- | A class for types whose values can be applied.
--   Instances are required to be uniquely determined by the applied and applied-to type.
class Applicable f a b | f a -> b where
  -- | Apply a value to another value, producing a result.
  ($*) :: f -> a -> b

instance Applicable (a -> b) a b where
  f $* x = f x

-- | A wrapper for values.
--   Can be applied to a function '(GHC.Types.->)', applying the function to the inner value.
newtype ApplyTo a = AppTo { unAppTo :: a } deriving (Generic, Data, Eq, Ord, Show, Read, Ix, Functor, Foldable, Traversable)

instance Applicable (ApplyTo a) (a -> b) b where
  AppTo x $* f = f x

-- | A wrapper for functions.
--   Can be applied to a 'Functor', 'fmap'-ing the function over the inner values.
newtype ApplyMap a b = AppMap { unAppMap :: a -> b } deriving Functor

instance Functor f => Applicable (ApplyMap a b) (f a) (f b) where
  AppMap f $* xa = f <$> xa

-- | A wrapper for functions in an applicative functor.
--   Can be applied to an 'Applicative' functor, '(<*>)'-ing it on it.
newtype ApplyAp f a b = AppAp { unAppAp :: f (a -> b) } deriving (Generic, Functor)

instance Applicative f => Applicable (ApplyAp f a b) (f a) (f b) where
  AppAp f $* xa = f <*> xa

-- | A wrapper for 'Control.Arrow.Kleisli' arrows.
--   Can be applied to a 'Monad', '(>>=)'-ing it on it.
newtype ApplyBind m a b = AppBind { unAppBind :: a -> m b } deriving (Generic, Functor)

instance Monad m => Applicable (ApplyBind m a b) (m a) (m b) where
  AppBind f $* xa = xa >>= f

-- | A wrapper for 'Semigroup' members, representing the associated group action.
--   Can be applied to another member, '(<>)'-ing them.
newtype GroupAction a = GrpAct { unGrpAct :: a } deriving (Generic, Data, Eq, Ord, Show, Read, Ix, Functor, Foldable, Traversable)

instance Semigroup a => Applicable (GroupAction a) a a where
  GrpAct a $* b = a <> b

-- | A wrapper for 'Bool's.
--   When applied to a value, uses the Church encoding of Booleans.
--   The Church encoding of Booleans is a binary function
--   that returns its first argument for 'True', and its second for 'False'.
newtype ChurchBool = ChBool { unChBool :: Bool } deriving (Generic, Data, Eq, Ord, Show, Read, Ix)

instance Applicable ChurchBool a (a -> a) where
  ($*) (ChBool True) t _ = t
  ($*) (ChBool False) _ f = f

-- | Update the 'Bool' in a 'ChurchBool'.
mapChBool :: (Bool -> Bool) -> ChurchBool -> ChurchBool
mapChBool f (ChBool x) = ChBool $ f x

-- | A wrapper for natural numbers (Approximated by 'Integral').
--   When applied to a value, uses the Church encoding of natural numbers.
--   Church numerals represent the number _n_ as a function that take another function and repeatedly applies it _n_ times.
newtype ChurchNumeral a = ChNum { unChNum :: a } deriving (Generic, Data, Eq, Ord, Show, Read, Ix, Functor, Foldable, Traversable)

instance Integral a => Applicable (ChurchNumeral a) (a -> a) (a -> a) where
  ($*) (ChNum n) f x = genericIndex (iterate f x) n

-- | A wrapper for tuples '(,)'.
--   When applied to a value, uses the Church encoding of tuples.
--   The Church encoding of tuples applies a function to the values inside a tuple.
newtype ChurchTuple a b = ChTup { unChTup :: (a, b) } deriving (Generic, Data, Eq, Ord, Show, Read, Ix, Functor, Foldable, Traversable, Bifunctor)

instance Applicable (ChurchTuple a b) (a -> b -> c) c where
  ChTup (x, y) $* f = f x y
