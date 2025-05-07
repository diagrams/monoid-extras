{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Coproduct
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The coproduct of two monoids.
--
-----------------------------------------------------------------------------

module Data.Monoid.Coproduct
       ( (:+:)
       , inL, inR
       , mappendL, mappendR
       , cop
       , killL, killR
       , toAltList
       , toReducedAltList
       , untangle
       , untangleSemi
       ) where

import Data.Function      (on)
import Data.Semigroup
import Data.Typeable

import Data.Monoid.Action
import Data.Monoid.SemiDirectProduct ( embed, inject, Semi, unSemi )
import Data.Tuple (swap)

-- | @m :+: n@ is the coproduct of monoids @m@ and @n@.  Values of
--   type @m :+: n@ consist of alternating lists of @m@ and @n@
--   values. The empty list is the identity, and composition is list
--   concatenation, with appropriate combining of adjacent elements
--   and removing identities when possible.
newtype m :+: n = MCo { unMCo :: [Either m n] }
  deriving (Typeable, Show)

instance (Eq m, Eq n, Monoid m, Monoid n) => Eq (m :+: n) where
  (==) = (==) `on` (normalizeEq . unMCo)

-- | Extract a monoid coproduct to a list of @Either@ values.  The
--   resulting list is guaranteed to be normalized, in the sense that
--   it will strictly alternate between @Left@ and @Right@.
toAltList :: (Semigroup m, Semigroup n) => (m :+: n) -> [Either m n]
toAltList (MCo ms) = normalize ms

-- | Extract a monoid coproduct to a list of @Either@ values.  The
--   resulting list is guaranteed to be normalized, in the sense that
--   it will strictly alternate between @Left@ and @Right@ and no identity
--   element from @m@ or @n@ will occur in the list.
toReducedAltList :: (Eq m, Eq n, Monoid m, Monoid n) => (m :+: n) -> [Either m n]
toReducedAltList (MCo ms) = normalizeEq ms

-- Normalize a list of @Either@ values by combining any consecutive
-- values of the same type.
normalize :: (Semigroup m, Semigroup n) => [Either m n] -> [Either m n]
normalize = \case
  (Left e1:Left e2 : es) -> normalize (Left (e1 <> e2) : es)
  (Right e1:Right e2:es) -> normalize (Right (e1 <> e2) : es)
  []  -> []
  (e:es) -> e : normalize es


-- Similar to @normalize@. In additin to combining consecutive values of the same
-- type it also removes the identities.
normalizeEq :: (Eq m, Eq n, Monoid m, Monoid n) => [Either m n] -> [Either m n]
normalizeEq es = until (all nonIdentity) reduce (normalize es)
  where
    reduce = normalize . filter nonIdentity
    nonIdentity e = e /= Left mempty && e /= Right mempty

-- For efficiency and simplicity, we implement it just as [Either m
-- n]: of course, this does not preserve the invariant of strictly
-- alternating types, but it doesn't really matter as long as we don't
-- let anyone inspect the internal representation.

-- | Universal map of the coproduct. The name @cop@ is an abbreviation
--   for copairing. Both functions in the signature should be monoid
--   homomorphisms. If they are general functions then tha coparining may
--   not be well defined in the sense that it may send equal elements to
--   unequal elements. This is also the reason why @cop@ is not the
--   @Data.Bifoldable.bifoldMap@ function even though they have the same
--   signature.
cop :: Monoid k => (m -> k) -> (n -> k) -> (m :+: n) -> k
f `cop` g = foldMap (either f g) . unMCo

-- | Injection from the left monoid into a coproduct.
inL :: m -> m :+: n
inL m = MCo [Left m]

-- | Injection from the right monoid into a coproduct.
inR :: n -> m :+: n
inR n = MCo [Right n]

-- | Prepend a value from the left monoid.
mappendL :: m -> m :+: n -> m :+: n
mappendL = mappend . inL

-- | Prepend a value from the right monoid.
mappendR :: n -> m :+: n -> m :+: n
mappendR = mappend . inR

instance Semigroup (m :+: n) where
  (MCo es1) <> (MCo es2) = MCo (es1 ++ es2)

-- | The coproduct of two monoids is itself a monoid.
instance Monoid (m :+: n) where
  mempty = MCo []
  mappend = (<>)

-- | @killR@ takes a value in a coproduct monoid and sends all the
--   values from the right monoid to the identity.
killR :: Monoid m => m :+: n -> m
killR = id `cop` const mempty

-- | @killL@ takes a value in a coproduct monoid and sends all the
--   values from the left monoid to the identity.
killL :: Monoid n => m :+: n -> n
killL = const mempty `cop` id

-- | The copairing of @embed@ and @inject@ homomorphisms into the
--   semidirect product. Note that @embed@ and @inject@ are monoid
--   homomorphisms. Therefore @untangleSemi@ is also a monoid homomorphism.
untangleSemi :: (Action m n, Monoid m, Monoid n) => m :+: n -> Semi n m
untangleSemi = embed `cop` inject

-- | Same as @untangleSemi@ but the result is uwrapped. Concretely, given
--   a value from a coproduct monoid where the left monoid has an
--   action on the right, and \"untangle\" it into a pair of values.  In
--   particular,
--
-- > m1 <> n1 <> m2 <> n2 <> m3 <> n3 <> ...
--
--   is sent to
--
-- > (m1 <> m2 <> m3 <> ..., (act m1 n1) <> (act (m1 <> m2) n2) <> (act (m1 <> m2 <> m3) n3) <> ...)
--
--   That is, before combining @n@ values, every @n@ value is acted on
--   by all the @m@ values to its left.
untangle :: (Action m n, Monoid m, Monoid n) => m :+: n -> (m,n)
untangle = swap . unSemi . untangleSemi

-- | Coproducts act on other things by having each of the components
--   act individually.
instance (Action m r, Action n r) => Action (m :+: n) r where
  act = appEndo . ((Endo . act) `cop` (Endo . act))
