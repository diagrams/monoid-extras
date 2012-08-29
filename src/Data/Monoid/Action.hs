{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Action
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Monoid and semigroup actions.
--
-----------------------------------------------------------------------------

module Data.Monoid.Action
       ( Action(..)
       ) where

import Data.Semigroup

------------------------------------------------------------
--  Monoid and semigroup actions
------------------------------------------------------------

-- | Type class for monoid (and semigroup) actions, where monoidal
--   values of type @m@ \"act\" on values of another type @s@.
--   Instances are required to satisfy the laws
--
--   * @act mempty = id@
--
--   * @act (m1 ``mappend`` m2) = act m1 . act m2@
--
--   Semigroup instances are required to satisfy the second law but with
--   '(<>)' instead of 'mappend'.  Additionally, if the type @s@ has
--   any algebraic structure, @act m@ should be a homomorphism.  For
--   example, if @s@ is also a monoid we should have @act m mempty =
--   mempty@ and @act m (s1 ``mappend`` s2) = (act m s1) ``mappend``
--   (act m s2)@.
--
--   By default, @act = const id@, so for a type @M@ which should have
--   no action on anything, it suffices to write
--
--   > instance Action M s
--
--   with no method implementations.
class Action m s where

  -- | Convert a value of type @m@ to an action on @s@ values.
  act :: m -> s -> s
  act = const id

-- | @Nothing@ acts as the identity; @Just m@ acts as @m@.
instance Action m s => Action (Option m) s where
  act (Option Nothing)  s = s
  act (Option (Just m)) s = act m s

-- | Actions operate elementwise on pairs.
instance (Action m a, Action m b) => Action m (a,b) where
  act m (a,b) = (act m a, act m b)

-- | Actions operate elementwise on triples.
instance (Action m a, Action m b, Action m c) => Action m (a,b,c) where
  act m (a,b,c) = (act m a, act m b, act m c)
