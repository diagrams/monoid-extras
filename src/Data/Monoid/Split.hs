{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Split
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Sometimes we want to accumulate values from some monoid, but have
-- the ability to introduce a \"split\" which separates values on
-- either side.  For example, in the diagrams graphics framework this
-- is used when accumulating transformations to be applied to
-- primitive diagrams: the 'freeze' operation introduces a split,
-- since only transformations occurring outside the freeze should be
-- applied to attributes.
--
-----------------------------------------------------------------------------

module Data.Monoid.Split
       ( Split(..), split, unsplit

       ) where

import Data.Semigroup

import Data.Monoid.Action

infix 5 :|

-- | A value of type @Split m@ is either a single @m@, or a pair of
--   @m@'s separated by a divider.
data Split m = M m
             | m :| m
  deriving (Show)

-- | If @m@ is a @Semigroup@, then @Split m@ is a semigroup which
--   combines values on either side of a split, keeping only the
--   rightmost split.
instance Semigroup m => Semigroup (Split m) where
  (M m1)       <> (M m2)       = M (m1 <> m2)
  (M m1)       <> (m1' :| m2)  = m1 <> m1'         :| m2
  (m1  :| m2)  <> (M m2')      = m1                :| m2 <> m2'
  (m11 :| m12) <> (m21 :| m22) = m11 <> m12 <> m21 :| m22

instance (Semigroup m, Monoid m) => Monoid (Split m) where
  mempty  = M mempty
  mappend = (<>)

-- | A convenient name for @mempty :| mempty@, so @a \<\> split \<\> b == a :| b@.
split :: Monoid m => Split m
split = mempty :| mempty

-- | \"Unsplit\" a split monoid value, combining the two values into
--   one (or returning the single value if there is no split).
unsplit :: Semigroup m => Split m -> m
unsplit (M m)      = m
unsplit (m1 :| m2) = m1 <> m2

-- | By default, the action of a split monoid is the same as for
--   the underlying monoid, as if the split were removed.
instance Action m n => Action (Split m) n where
  act (M m) n      = act m n
  act (m1 :| m2) n = act m1 (act m2 n)
