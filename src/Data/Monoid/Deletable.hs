{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Deletable
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A monoid transformer that allows deleting information from a
-- concatenation of monoidal values.
--
-----------------------------------------------------------------------------

module Data.Monoid.Deletable
       ( Deletable(..)

       , unDelete, toDeletable

       , deleteL, deleteR

       ) where

import Data.Data
import Data.Foldable
import Data.Traversable
import Data.Semigroup

-- | If @m@ is a 'Monoid', then @Deletable m@ (intuitively speaking)
--   adds two distinguished new elements @[@ and @]@, such that an
--   occurrence of [ \"deletes\" everything from it to the next ]. For
--   example,
--
--   > abc[def]gh == abcgh
--
--   This is all you really need to know to /use/ @Deletable m@
--   values; to understand the actual implementation, read on.
--
--   To properly deal with nesting and associativity we need to be
--   able to assign meanings to things like @[[@, @][@, and so on. (We
--   cannot just define, say, @[[ == [@, since then @([[)] == [] ==
--   id@ but @[([]) == [id == [@.)  Formally, elements of @Deletable
--   m@ are triples of the form (r, m, l) representing words @]^r m
--   [^l@.  When combining two triples (r1, m1, l1) and (r2, m2, l2)
--   there are three cases:
--
--   * If l1 == r2 then the [s from the left and ]s from the right
--     exactly cancel, and we are left with (r1, m1 \<\> m2, l2).
--
--   * If l1 < r2 then all of the [s cancel with some of the ]s, but
--     m1 is still inside the remaining ]s and is deleted, yielding (r1
--     + r2 - l1, m2, l2)
--
--   * The remaining case is symmetric with the second.

data Deletable m = Deletable Int m Int
  deriving (Data, Typeable, Show, Read, Functor, Foldable, Traversable)

-- | Project the wrapped value out of a `Deletable` value.
unDelete :: Deletable m -> m
unDelete (Deletable _ m _) = m

-- | Inject a value into a `Deletable` wrapper.  Satisfies the
--   property
--
-- > unDelete . toDeletable === id
--
toDeletable :: m -> Deletable m
toDeletable m = Deletable 0 m 0

instance Semigroup m => Semigroup (Deletable m) where
  (Deletable r1 m1 l1) <> (Deletable r2 m2 l2)
    | l1 == r2  = Deletable r1 (m1 <> m2) l2
    | l1 <  r2  = Deletable (r1 + r2 - l1) m2 l2
    | otherwise = Deletable r1 m1 (l2 + l1 - r2)

instance (Semigroup m, Monoid m) => Monoid (Deletable m) where
  mempty = Deletable 0 mempty 0
  mappend = (<>)

-- | A \"left bracket\", which causes everything between it and the
--   next right bracket to be deleted.
deleteL :: Monoid m => Deletable m
deleteL = Deletable 0 mempty 1

-- | A \"right bracket\", denoting the end of the section that should
--   be deleted.
deleteR :: Monoid m => Deletable m
deleteR = Deletable 1 mempty 0
