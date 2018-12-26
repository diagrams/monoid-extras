{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances       #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.MList
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Heterogeneous lists of monoids.
--
-----------------------------------------------------------------------------
module Data.Monoid.MList
       ( -- * Heterogeneous monoidal lists

         -- $mlist

         (:::), (*:)

       , MList(..)

         -- * Accessing embedded values
       , (:>:)(..)

         -- * Monoid actions of heterogeneous lists

         -- $mlist-actions

       , SM(..)
       ) where

import           Control.Arrow
import           Data.Monoid.Action.LeftAction
import           Data.Semigroup

-- $mlist
--
-- The idea of /heterogeneous lists/ has been around for a long time.
-- Here, we adopt heterogeneous lists where the element types are all
-- monoids: this allows us to leave out identity values, so that a
-- heterogeneous list containing only a single non-identity value can
-- be created without incurring constraints due to all the other
-- types, by leaving all the other values out.

infixr 5 :::
infixr 5 *:

type a ::: l = (Option a, l)

(*:) :: a -> l -> a ::: l
a *: l = (Option (Just a), l)

-- MList -----------------------------------

-- | Type class for heterogeneous monoidal lists, with a single method
--   allowing construction of an empty list.
class MList l where
  -- | The /empty/ heterogeneous list of type @l@. Of course, @empty
  -- == 'mempty'@, but unlike 'mempty', @empty@ does not require
  -- 'Monoid' constraints on all the elements of @l@.
  empty   :: l

instance MList () where
  empty     = ()

instance MList l => MList (a ::: l) where
  empty   = (Option Nothing, empty)

-- Embedding -------------------------------------------

-- | The relation @l :>: a@ holds when @a@ is the type of an element
--   in @l@.  For example,  @(Char ::: Int ::: Bool ::: Nil) :>: Int@.
class l :>: a where
  -- | Inject a value into an otherwise empty heterogeneous list.
  inj  :: a -> l

  -- | Get the value of type @a@ from a heterogeneous list, if there
  --   is one.
  get  :: l -> Option a

  -- | Alter the value of type @a@ by applying the given function to it.
  alt  :: (Option a -> Option a) -> l -> l

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} MList t => (:>:) (a ::: t) a where
#else
instance MList t => (:>:) (a ::: t) a where
#endif
  inj a = (Option (Just a), empty)
  get   = fst
  alt   = first

instance (t :>: a) => (:>:) (b ::: t) a where
  inj a = (Option Nothing, inj a)
  get   = get . snd
  alt   = second . alt

-- Monoid left actions -----------------------------------------

-- $mlist-actions
-- Monoidal heterogeneous lists may act on one another as you would
-- expect, with each element in the first list acting on each in the
-- second.  Unfortunately, coding this up in type class instances is a
-- bit fiddly.

-- | @SM@, an abbreviation for \"single monoid\" (as opposed to a
--   heterogeneous list of monoids), is only used internally to help
--   guide instance selection when defining the left action of
--   heterogeneous monoidal lists on each other.
newtype SM m = SM m
               deriving Show

instance (LeftAction (SM a) l2, LeftAction l1 l2) => LeftAction (a, l1) l2 where
  leftAct (a,l) = leftAct (SM a) . leftAct l

instance LeftAction (SM a) () where
  leftAct _ _ = ()

instance (LeftAction a a', LeftAction (SM a) l) => LeftAction (SM a) (Option a', l) where
  leftAct (SM a) (Option Nothing,   l) = (Option Nothing, leftAct (SM a) l)
  leftAct (SM a) (Option (Just a'), l) = (Option (Just (leftAct a a')), leftAct (SM a) l)
