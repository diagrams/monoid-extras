{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Endomorphism
-- Copyright   :  (c) 2013-2015 diagrams-core team (see LICENSE)
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
import           Data.Monoid       (Monoid(..))
import           Data.Semigroup    (Semigroup(..))
import           Data.Semigroupoid
import           Prelude           (Show)

-- | An 'Endomorphism' in a given 'Category' is a morphism from some
--   object to itself.  The set of endomorphisms for a particular
--   object form a monoid, with composition as the combining operation
--   and the identity morphism as the identity element.
newtype Endomorphism k a = Endomorphism {getEndomorphism :: k a a}

deriving instance Show (k a a) => Show (Endomorphism k a)

instance Semigroupoid k => Semigroup (Endomorphism k a) where
  Endomorphism a <> Endomorphism b = Endomorphism (a `o` b)

instance (Semigroupoid k, Category k) => Monoid (Endomorphism k a) where
  mempty = Endomorphism id
#if !MIN_VERSION_base(4,11,0)
  Endomorphism a `mappend` Endomorphism b = Endomorphism (a . b)
#endif

instance (Category k, Groupoid k) => Group (Endomorphism k a) where
  invert (Endomorphism a) = Endomorphism (inv a)
