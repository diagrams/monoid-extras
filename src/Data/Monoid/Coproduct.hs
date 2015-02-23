{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

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
       , killL, killR
       , untangle

       ) where

import Data.Either        (lefts, rights)
import Data.Semigroup
import Data.Typeable

import Data.Monoid.Action

-- | @m :+: n@ is the coproduct of monoids @m@ and @n@.  Values of
--   type @m :+: n@ consist of alternating lists of @m@ and @n@
--   values.  The empty list is the identity, and composition is list
--   concatenation, with appropriate combining of adjacent elements
--   when possible.
newtype m :+: n = MCo { unMCo :: [Either m n] }
  deriving (Typeable, Show)

-- For efficiency and simplicity, we implement it just as [Either m
-- n]: of course, this does not preserve the invariant of strictly
-- alternating types, but it doesn't really matter as long as we don't
-- let anyone inspect the internal representation.

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

{-
normalize :: (Monoid m, Monoid n) => m :+: n -> m :+: n
normalize (MCo es) = MCo (normalize' es)
  where normalize' []  = []
        normalize' [e] = [e]
        normalize' (Left e1:Left e2 : es) = normalize' (Left (e1 <> e2) : es)
        normalize' (Left e1:es) = Left e1 : normalize' es
        normalize' (Right e1:Right e2:es) = normalize' (Right (e1 <> e2) : es)
        normalize' (Right e1:es) = Right e1 : normalize' es
-}

instance Semigroup (m :+: n) where
  (MCo es1) <> (MCo es2) = MCo (es1 ++ es2)

-- | The coproduct of two monoids is itself a monoid.
instance Monoid (m :+: n) where
  mempty = MCo []
  mappend = (<>)

-- | @killR@ takes a value in a coproduct monoid and sends all the
--   values from the right monoid to the identity.
killR :: Monoid m => m :+: n -> m
killR = mconcat . lefts . unMCo

-- | @killL@ takes a value in a coproduct monoid and sends all the
--   values from the left monoid to the identity.
killL :: Monoid n => m :+: n -> n
killL = mconcat . rights . unMCo

-- | Take a value from a coproduct monoid where the left monoid has an
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
untangle (MCo elts) = untangle' mempty elts
  where untangle' cur [] = cur
        untangle' (curM, curN) (Left m : elts')  = untangle' (curM `mappend` m, curN) elts'
        untangle' (curM, curN) (Right n : elts') = untangle' (curM, curN `mappend` act curM n) elts'

-- | Coproducts act on other things by having each of the components
--   act individually.
instance (Action m r, Action n r) => Action (m :+: n) r where
  act = appEndo . mconcat . map (Endo . either act act) . unMCo
