{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Inf
-- Copyright   :  (c) 2012-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Make semigroups under 'min' or 'max' into monoids by adjoining an
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

import           Control.Applicative (Applicative(..), liftA2)
import           Data.Data
import           Data.Semigroup
import           Prelude             hiding (maximum, minimum)
import qualified Prelude             as P

import           Data.Foldable       (Foldable)
import           Data.Traversable    (Traversable)

-- | Type index indicating positive infinity.
data Pos
-- | Type index indicating negative infinity.
data Neg

-- | @Inf p a@ represents the type 'a' extended with a new "infinite"
--   value, which is treated as either positive or negative infinity
--   depending on the type index 'p'.  This type exists mostly for its
--   'Ord', 'Semigroup', and 'Monoid' instances.
data Inf p a = Infinity | Finite a
  deriving (Data, Typeable, Show, Read, Eq, Functor, Foldable,
            Traversable)

-- | The type 'a' extended with positive infinity.
type PosInf a = Inf Pos a

-- | The type 'a' extended with negative infinity.
type NegInf a = Inf Neg a

-- | Positive infinity is greater than any finite value.
instance Ord a => Ord (Inf Pos a) where
  compare Infinity Infinity = EQ
  compare Infinity Finite{} = GT
  compare Finite{} Infinity = LT
  compare (Finite a) (Finite b) = compare a b

-- | Negative infinity is less than any finite value.
instance Ord a => Ord (Inf Neg a) where
  compare Infinity Infinity = EQ
  compare Infinity Finite{} = LT
  compare Finite{} Infinity = GT
  compare (Finite a) (Finite b) = compare a b

-- | An ordered type extended with positive infinity is a semigroup
--   under 'min'.
instance Ord a => Semigroup (Inf Pos a) where
  (<>) = min

-- | An ordered type extended with negative infinity is a semigroup
--   under 'max'.
instance Ord a => Semigroup (Inf Neg a) where
  (<>) = max

-- | An ordered type extended with positive infinity is a monoid under
--   'min', with positive infinity as the identity element.
instance Ord a => Monoid (Inf Pos a) where
  mempty = Infinity
  mappend = (<>)

-- | An ordered type extended with negative infinity is a monoid under
--   'max', with negative infinity as the identity element.
instance Ord a => Monoid (Inf Neg a) where
  mempty = Infinity
  mappend = (<>)

instance Applicative (Inf p) where
    pure = Finite
    Infinity <*> _ = Infinity
    _ <*> Infinity = Infinity
    Finite f <*> Finite x = Finite $ f x

instance Monad (Inf p) where
    Infinity >>= _ = Infinity
    Finite x >>= f = f x
    return = pure

instance Bounded a => Bounded (NegInf a) where
    minBound = Infinity
    maxBound = Finite maxBound

instance Bounded a => Bounded (PosInf a) where
    minBound = Finite minBound
    maxBound = Infinity

-- | Find the minimum of a list of values.  Returns positive infinity
--   iff the list is empty.
minimum :: Ord a => [a] -> PosInf a
minimum xs = P.minimum (Infinity : map Finite xs)

-- | Find the maximum of a list of values.  Returns negative infinity
--   iff the list is empty.
maximum :: Ord a => [a] -> NegInf a
maximum xs = P.maximum (Infinity : map Finite xs)

-- | Positive infinity.
posInfty :: PosInf a

-- | Negative infinity.
negInfty :: NegInf a

-- | Embed a finite value into the space of such values extended with
--   positive infinity.
posFinite :: a -> PosInf a

-- | Embed a finite value into the space of such values extended with
--   negative infinity.
negFinite :: a -> NegInf a

posInfty = Infinity
negInfty = Infinity
posFinite = Finite
negFinite = Finite
