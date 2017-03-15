{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Priority
-- Copyright   :  (c) 2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for representing values with a priority level, indicating which 
-- values should take precedence over others, along with corresponding 
-- 'Semigroup' and 'Monoid' instances. This is effectively a generalized 
-- version of 'Data.Monoid.Recommend.Recommend', which is equivalent to
-- @Priority Bool a@.
--
-----------------------------------------------------------------------------

module Data.Monoid.Priority
       ( Priority(..), priority
       , fromPriority, priorityLevel
       , PriorityAction(..), priorityAction
       ) where

import Data.Data
import Data.Foldable
import Data.Semigroup
import Data.Traversable

import Data.Monoid.Action

-- | A value of type @Priority p a@ consists of a value of type @a@ along with
--   a priority level of type @p@.
data Priority p a = Priority !p !a
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Construct a @Priority p a@ value
priority :: p -> a -> Priority p a
priority = Priority

-- | Extract the value of type @a@ wrapped in @Priority p a@.
fromPriority :: Priority p a -> a
fromPriority (Priority _ x) = x

-- | Extract the priority level of type @p@ from a @Priority p a@.
priorityLevel :: Priority p a -> p
priorityLevel (Priority p _) = p

-- | If one value has a higher priority, it will override the other. If the 
--   priorities are equal, the values are combined using the underlying 
--   @Semigroup@ instance.
instance (Ord p, Semigroup a) => Semigroup (Priority p a) where
    Priority p x <> Priority q y 
        | p > q     = Priority p x
        | p < q     = Priority q y
        | otherwise = Priority p (x <> y)


-- | The @Monoid@ instance adds an additional @Bounded@ constraint, used to give
--   @mempty@ the lowest priority.
instance (Bounded p, Ord p, Semigroup a, Monoid a) => Monoid (Priority p a) where
    mempty = Priority minBound mempty
    mappend = (<>)


-- | An action on @Priority@ values.
newtype PriorityAction p a = PriorityAction (Priority p (Endo a))

-- | Construct a @PriorityAction p a@ value
priorityAction :: p -> (a -> a) -> PriorityAction p a
priorityAction p f = PriorityAction (Priority p (Endo f))

-- | A composite @PriorityAction@ takes the least priority of its component 
--   actions.
instance (Ord p) => Semigroup (PriorityAction p a) where
    PriorityAction (Priority p f) <> PriorityAction (Priority q g) 
        = PriorityAction (Priority (min p q) (f <> g))

instance (Bounded p, Ord p) => Monoid (PriorityAction p a) where
    mempty = PriorityAction (Priority minBound mempty)
    mappend = (<>)

-- | Actions modify values with equal or lower priority, leaving 
--   higher-priority values unchanged.
instance (p ~ q, a ~ b, Ord p) => Action (PriorityAction p a) (Priority q b) where
    act (PriorityAction (Priority p f)) px@(Priority q x) 
        | p >= q    = Priority q (appEndo f x)
        | otherwise = px


