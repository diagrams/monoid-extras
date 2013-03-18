{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.PosInf
-- Copyright   :  (c) 2012 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Make a semigroup under 'min' into a monoid by adjoining an element
-- corresponding to positive infinity.
--
-----------------------------------------------------------------------------

module Data.Monoid.Inf
       ( Inf(..)
       , minimum, maximum
       , posInfty, negInfty
       , posFinite, negFinite
       ) where

import           Data.Semigroup
import qualified Prelude as P
import           Prelude hiding (minimum, maximum)

data Pos
data Neg

-- | Affix a (positive or negative) infinity to a type.
data Inf p a = Infinity | Finite a
  deriving (Eq, Show, Read)

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
