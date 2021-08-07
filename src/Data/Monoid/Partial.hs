{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedLists #-}
{-# language DefaultSignatures #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}
{-# language DeriveAnyClass #-}

{-|
Module      :  Data.Monoid.Partial
Copyright   :  (c) 2021 Julia Path
License     :  BSD-style (see LICENSE)
Maintainer  :  diagrams-discuss@googlegroups.com

This module defines partial semigroups and monoids, as well as free semigroups
and monoids over these.

One usecase of this is if you have some type @S@ and you want to have a list of
@S@, however some values of @S@ should actually be combined, such that you do
not have two such combinable elements next to each other.

This came into live to deal with the following case: In XML you may describe the
contents of an element as being a list of @Node@s, where a @Node@ may be an
element, a text node or others. Now two text nodes can be combined by combining
their text content; other node types however can't. Therefore @Node@ is a
partial semigroup. Furthermore we may define the combination of the empty text
node with any other non-text node, by just taking the other node as a result.
Thus @Node@ is a partial monoid. We can now use @'FreeSemigroup' Node@ which is
also the free monoid over a partial monoid to represent a list of @Node@s with no
two consecutive nodes being text nodes.
-}
module Data.Monoid.Partial
  ( PartialSemigroup(..), PartialMonoid(..),
    FreeMonoid, singleton, foldMap, toSeq,
    FreeSemigroup, singletonS, foldMap1, toSeqS
  ) where

import Prelude
import Data.Semigroup.Foldable (Foldable1, foldMap1)

import Data.Sequence hiding (singleton)
import qualified Data.Sequence as Seq
import GHC.Exts (IsList(..))
import Control.DeepSeq

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Functor.Identity (Identity(..))
import Control.Applicative (Alternative)
import Data.Semigroup (Any, All, Endo, Dual(..), Last, First, Min, Max, Sum, Product)
import Data.Monoid (Alt, Ap(..))
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty)

{- |
A /partial semigroup/ is an associative operation which may not be defined on
all pairs of elements. We say @'pappend' s1 s2@ is /defined/ if it is 'Just',
in this case we call @s1@, @s2@ /combinable/.

__Laws__

@
'pappend' \<$> ('pappend' x y) \<*> z = 'pappend' \<$> x \<*> ('pappend' y z)
@

__Homomorphisms__

We define a /partial-semigroup homorphism/ to be a function @f : S -> T@
where @S@ and @T@ are partial-semigroups, such that

@
s1, s2 combinable ⇒ f s1 `'pappend'`  f s2 = f \<$> s1 `'pappend'` s2 ∀s1, s2 :: S
@

We use a hyphen between partial and semigroup here, to disambiguate from a
partial semigroup homomorphism, which could also be a partial homomorphism
between two semigroups.

Note that even if @s1 `'pappend'` s2@ is not defined, @f s1 `'pappend'` f s2@
may be. We say that a partial-semigroup homomorphism /reflects definedness/ or
simply is /reflective/ if

@
f s1, f s2 combinable ⇒ s1, s2 combinable ∀s1, s2 :: S
@

This is the case if and only if @f \<$> s1 `'pappend'` s2 = f s1 `'pappend'` f s2@
for all @s1, s2 :: S@.
-}
class PartialSemigroup a where
  pappend :: a -> a -> Maybe a

  default pappend :: Semigroup a => a -> a -> Maybe a
  x `pappend` y = Just $ x <> y

{- |
A /partial monoid/ is a partial semigroup with a unit. 'pappend' with the
unit is always defined and the identity.

__Laws__

@
x `'pappend'` 'pempty'  = 'Just' x = 'pappend' 'pempty' x
@

__Homomorphisms__

A /partial-monoid homomorphism/ is a partial-semigroup homomorphism which
preserves the unit.
-}
class PartialSemigroup a => PartialMonoid a where
  pempty :: a

  default pempty :: Monoid a => a
  pempty = mempty

deriving newtype instance PartialSemigroup a => PartialSemigroup (Identity a)
deriving newtype instance PartialMonoid a => PartialMonoid (Identity a)

instance PartialSemigroup a => PartialSemigroup (Maybe a) where
  x `pappend` Nothing = Just x
  Nothing `pappend` y = Just y
  Just x `pappend` Just y = Just <$> x `pappend` y
-- | /Free partial monoid over a partial-semigroup/
instance PartialSemigroup a => PartialMonoid (Maybe a) where
  pempty = Nothing

instance (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (a, b) where
  (x1, x2) `pappend` (y1, y2) = do
    a <- x1 `pappend` y1
    b <- x2 `pappend` y2
    Just (a, b)
instance (PartialMonoid a, PartialMonoid b) => PartialMonoid (a, b) where
  pempty = (pempty, pempty)

instance (PartialSemigroup a, PartialSemigroup b, PartialSemigroup c) => PartialSemigroup (a, b, c) where
  (x1, x2, x3) `pappend` (y1, y2, y3) = do
    a <- x1 `pappend` y1
    b <- x2 `pappend` y2
    c <- x3 `pappend` y3
    Just (a, b, c)
instance (PartialMonoid a, PartialMonoid b, PartialMonoid c) => PartialMonoid (a, b, c) where
  pempty = (pempty, pempty, pempty)

instance (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (Either a b) where
  Left x `pappend` Left y = Left <$> x `pappend` y
  Right x `pappend` Right y = Right <$> x `pappend` y
  _ `pappend` _ = Nothing

instance PartialSemigroup a => PartialSemigroup (Dual a) where
  Dual x `pappend` Dual y = Dual <$> y `pappend` x
instance PartialMonoid a => PartialMonoid (Dual a) where
  pempty = Dual pempty

{- |
The /free monoid over a partial semigroup/:

For any partial-semigroup homomorphism @f :: a -> m@ to some monoid @m@ we
get, 'foldMap' @f@ a monoid homomorphism and

@
'foldMap' f . 'singleton' = f
@

A value in @'FreeMonoid' S@ for some partial-semigroup @S@ is a possibly empty
list @[s₁, s₂, …, sₙ]@ with no @sᵢ@, @sᵢ₊₁@ combinable. Thus if @S@ has no
combinable elements these are just the finite lists of elements of @S@.
-}
newtype FreeMonoid a = FreeMonoid (Seq a)
  deriving newtype (Show, Eq, Ord, Foldable, NFData)

instance PartialSemigroup a => PartialSemigroup (FreeMonoid a)
instance PartialSemigroup a => PartialMonoid (FreeMonoid a)

instance PartialSemigroup a => Semigroup (FreeMonoid a) where
  FreeMonoid xs <> FreeMonoid ys = case (viewr xs, viewl ys) of
    (EmptyR, _) -> FreeMonoid ys
    (_, EmptyL) -> FreeMonoid xs
    (xs' :> x, y :< ys')
      | Just xy <- (x `pappend` y) -> FreeMonoid $ xs' <> [xy] <> ys'
      | otherwise -> FreeMonoid $ xs <> ys
instance PartialSemigroup a => Monoid (FreeMonoid a) where
  mempty = FreeMonoid mempty

instance PartialSemigroup a => IsList (FreeMonoid a) where
  type Item (FreeMonoid a) = a

  fromList xs = foldMap singleton xs
  toList (FreeMonoid xs) = toList xs

{- |
'singleton' is the universal partial-semigroup homomorphism from a partial
semigroup into its free monoid.
-}
singleton :: a -> FreeMonoid a
singleton a = FreeMonoid $ Seq.singleton a

{- | Underneath @'FreeMonoid' a@ is just @'Seq' a@. -}
toSeq :: FreeMonoid a -> Seq a
toSeq (FreeMonoid xs) = xs

{- |
The /free semigroup over a partial semigroup/:

For any partial-semigroup homomorphism @f :: a -> s@ to some semigroup @s@ we
get, 'foldMap1' @f@ a semigroup homomorphism and

@
'foldMap1' f . 'singletonS' = f
@

Furthermore, this also turns out to be the /free monoid over a partial monoid/:

For any partial-monoid homomorphism @f :: a -> m@ to some monoid @m@ we
get, 'foldMap' @f@ a monoid homomorphism and

@
'foldMap' f . 'singletonS' = f
@

A value in @'FreeSemigroup' S@ for some partial-semigroup @S@ is a non-empty
list @[s₁, s₂, …, sₙ]@ with no @sᵢ@, @sᵢ₊₁@ combinable.
-}
newtype FreeSemigroup a = FreeSemigroup (Seq a)
  deriving newtype (Show, Eq, Ord, Foldable, NFData)
  deriving (Semigroup, PartialSemigroup, PartialMonoid) via FreeMonoid a
  deriving anyclass Foldable1

instance PartialMonoid a => Monoid (FreeSemigroup a) where
  mempty = singletonS pempty

instance PartialMonoid a => IsList (FreeSemigroup a) where
  type Item (FreeSemigroup a) = a

  fromList xs = foldMap singletonS xs
  toList (FreeSemigroup xs) = toList xs

{- |
'singletonS' is the universal partial-semigroup homomorphism from a partial
semigroup into its free semigroup.

Furthermore 'singletonS' is the universal partial-monoid homomorphism from a
partial monoid into its free monoid.
-}
singletonS :: a -> FreeSemigroup a
singletonS a = FreeSemigroup $ Seq.singleton a

{- | Underneath @'FreeSemigroup' a@ is just @'Seq' a@. -}
toSeqS :: FreeSemigroup a -> Seq a
toSeqS (FreeSemigroup xs) = xs

-- Total Instances

instance PartialSemigroup ()
instance PartialMonoid ()
instance PartialSemigroup Void
instance PartialSemigroup [a]
instance PartialMonoid [a]
instance PartialSemigroup (Seq a)
instance PartialMonoid (Seq a)
instance PartialSemigroup BS.ByteString
instance PartialMonoid BS.ByteString
instance PartialSemigroup LBS.ByteString
instance PartialMonoid LBS.ByteString
instance PartialSemigroup T.Text
instance PartialMonoid T.Text
instance PartialSemigroup LT.Text
instance PartialMonoid LT.Text
instance PartialSemigroup (NonEmpty a)
instance PartialSemigroup (Endo a)
instance PartialMonoid (Endo a)
instance PartialSemigroup Any
instance PartialMonoid Any
instance PartialSemigroup All
instance PartialMonoid All
instance PartialSemigroup (First a)
instance PartialSemigroup (Last a)
instance Ord a => PartialSemigroup (Min a)
instance (Ord a, Bounded a) => PartialMonoid (Min a)
instance Ord a => PartialSemigroup (Max a)
instance (Ord a, Bounded a) => PartialMonoid (Max a)
instance Num a => PartialSemigroup (Sum a)
instance Num a => PartialMonoid (Sum a)
instance Num a => PartialSemigroup (Product a)
instance Num a => PartialMonoid (Product a)
instance Alternative f => PartialSemigroup (Alt f a)
instance Alternative f => PartialMonoid (Alt f a)
instance (Semigroup a, Applicative f) => PartialSemigroup (Ap f a)
instance (Monoid a, Applicative f) => PartialMonoid (Ap f a)
