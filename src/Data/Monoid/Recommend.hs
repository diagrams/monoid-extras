{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Recommend
-- Copyright   :  (c) 2012-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for representing values with an additional bit saying
-- whether the value is \"just a recommendation\" (to be used only if
-- nothing better comes along) or a \"committment\" (to certainly be
-- used, overriding merely recommended values), along with
-- corresponding @Semigroup@ and @Monoid@ instances.
--
-----------------------------------------------------------------------------

module Data.Monoid.Recommend
       ( Recommend(..)
       , getRecommend
       ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable
import           Data.Traversable
#endif

import           Data.Data
import           Data.Semigroup

-- | A value of type @Recommend a@ consists of a value of type @a@
--   wrapped up in one of two constructors.  The @Recommend@
--   constructor indicates a \"non-committal recommendation\"---that
--   is, the given value should be used if no other/better values are
--   available.  The @Commit@ constructor indicates a
--   \"commitment\"---a value which should definitely be used,
--   overriding any @Recommend@ed values.
data Recommend a = Recommend a
                 | Commit a
  deriving (Show, Read, Functor, Eq, Ord, Typeable, Data, Foldable, Traversable)

-- | Extract the value of type @a@ wrapped in @Recommend a@.
getRecommend :: Recommend a -> a
getRecommend (Recommend a) = a
getRecommend (Commit a)    = a

-- | 'Commit' overrides 'Recommend'. Two values wrapped in the same
--   constructor (both 'Recommend' or both 'Commit') are combined
--   according to the underlying @Semigroup@ instance.
instance Semigroup a => Semigroup (Recommend a) where
  Recommend a <> Recommend b = Recommend (a <> b)
  Recommend _ <> Commit b    = Commit b
  Commit a    <> Recommend _ = Commit a
  Commit a    <> Commit b    = Commit (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Recommend a) where
  mappend = (<>)
  mempty  = Recommend mempty
