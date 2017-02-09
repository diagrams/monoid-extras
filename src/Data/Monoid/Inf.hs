{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
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

import           Data.Data
import           Data.Semigroup
import           Prelude          hiding (maximum, minimum)
import qualified Prelude          as P

import           Data.Foldable    (Foldable)
import           Data.Traversable (Traversable)

data Pos
data Neg

data Inf p a = Infinity | Finite a
  deriving (Data, Typeable, Show, Read, Eq, Functor, Foldable,
            Traversable)

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

instance Functor (Inf p) where
    fmap f Infinity = Infinity
    fmap f (Finite x) = Finite $ f x

instance Applicative (Inf p) where
    pure = Finite
    Infinity <*> _ = Infinity
    _ <*> Infinity = Infinity
    Finite f <*> Finite x = Finite $ f x

instance Monad (Inf p) where
    Infinity >>= _ = Infinity
    Finite x >>= f = Finite $ f x

instance Bounded a => Bounded (NegInf a) where
    minBound = Infinity
    maxBound = Finite maxBound

instance Bounded a => Bounded (Max a) where
    minBound = Finite minBound
    maxBound = Infinity

instance Num a => Num (Inf p a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = Finite . fromInteger
    negate = fmap negate

instance Fractional a => Fractional (Inf p a) where
    fromRational = Finite . fromRational
    recip = fmap recip

instance Floating a => Floating (Inf p a) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

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
