-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Endomorphism
-- Copyright   :  (c) 2013 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The monoid of endomorphisms over any 'Category'.
--
-----------------------------------------------------------------------------

module Data.Monoid.Endomorphism
    ( Endomorphism(..)
    )
    where

import           Control.Category
import           Data.Group
import           Data.Groupoid
import           Data.Semigroup
import           Data.Semigroupoid
import           Prelude           hiding (id, (.))

-- | An 'Endomorphism' in a given 'Category' is a morphism from some
--   object to itself.  The set of endomorphisms for a particular
--   object form a monoid, with composition as the combining operation
--   and the identity morphism as the identity element.
newtype Endomorphism k a = Endomorphism {getEndomorphism :: k a a}

instance Semigroupoid k => Semigroup (Endomorphism k a) where
  Endomorphism a <> Endomorphism b = Endomorphism (a `o` b)

instance Category k => Monoid (Endomorphism k a) where
  mempty = Endomorphism id
  Endomorphism a `mappend` Endomorphism b = Endomorphism (a . b)

instance (Category k, Groupoid k) => Group (Endomorphism k a) where
  invert (Endomorphism a) = Endomorphism (inv a)