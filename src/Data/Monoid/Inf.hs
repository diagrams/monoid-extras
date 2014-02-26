{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Inf
-- Copyright   :  (c) 2012 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Make semigroup under 'min' or 'max' into monoids by adjoining an
-- element corresponding to infinity (positive or negative,
-- respectively). These types are similar to @Option (Min a)@ and
-- @Option (Max a)@ respectively, except that the 'Ord' instance
-- matches the 'Monoid' instance.
--
-----------------------------------------------------------------------------

module Data.Monoid.Inf
       ( Inf(..)
       , Pos, Neg
       , PosInf, NegInf
       , minimum, maximum
       -- * Type-restricted constructors
       , posInfty, negInfty
       , posFinite, negFinite
       ) where

import           Data.Semigroup
import           Prelude          hiding (maximum, minimum)
import qualified Prelude          as P

import           Data.Foldable    (Foldable)
import           Data.Traversable (Traversable)

data Pos
data Neg

data Inf p a = Infinity | Finite a
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type PosInf a = Inf Pos a
type NegInf a = Inf Neg a

instance Ord a => Ord (Inf Pos a) where
  compare Infinity Infinity = EQ
  compare Infinity Finite{} = GT
  compare Finite{} Infinity = LT
  compare (Finite a) (Finite b) = compare a b

instance Ord a => Ord (Inf Neg a) where
  compare Infinity Infinity = EQ
  compare Infinity Finite{} = LT
  compare Finite{} Infinity = GT
  compare (Finite a) (Finite b) = compare a b

instance Ord a => Semigroup (Inf Pos a) where
  (<>) = min

instance Ord a => Semigroup (Inf Neg a) where
  (<>) = max

instance Ord a => Monoid (Inf Pos a) where
  mempty = Infinity
  mappend = (<>)

instance Ord a => Monoid (Inf Neg a) where
  mempty = Infinity
  mappend = (<>)

minimum :: Ord a => [a] -> PosInf a
minimum xs = P.minimum (Infinity : map Finite xs)

maximum :: Ord a => [a] -> NegInf a
maximum xs = P.maximum (Infinity : map Finite xs)

posInfty :: PosInf a
negInfty :: NegInf a
posFinite :: a -> PosInf a
negFinite :: a -> NegInf a

posInfty = Infinity
negInfty = Infinity
posFinite = Finite
negFinite = Finite
