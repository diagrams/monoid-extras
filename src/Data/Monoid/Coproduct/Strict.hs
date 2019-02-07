{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Coproduct.Strict
-- Copyright   :  (c) 2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A strict coproduct of two monoids.
--
-----------------------------------------------------------------------------

module Data.Monoid.Coproduct.Strict
  (
    -- * Coproduct
    (:+:)
  , inL, inR
  , prependL, prependR
  , killL, killR
  , untangle

  -- ** Lenses
  , untangled
  , _L
  , _R

  ) where

import           Data.Monoid.Action
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Prelude

-- Internal strict version of 'Data.Semigroup.Option'
data Possible a = Only !a | Nought

instance Semigroup a => Semigroup (Possible a) where
  Only a <> Only b = Only (a <> b)
  Nought <> b      = b
  a      <> _      = a
  {-# INLINE (<>) #-}

instance Semigroup a => Monoid (Possible a) where
  mempty = Nought
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

-- | @m :+: n@ is the coproduct of monoids @m@ and @n@. Concatentation
--   is equivilent to
--
-- @
-- (m1 :+: n1) <> (m2 :+: n2) = (m1 <> m2) :+: (n1 <> act m1 n2)@
-- @
--
--   but has a more efficient internal implimentation.
data m :+: n = C !(Possible n) !(Possible m) !(Possible n)
-- The left n already has the action m applied. The right n still needs
-- m applied, but it kept there incase more n comes to reduce the number
-- of actions that need to be applied.

instance (Action m n, Monoid m, Monoid' n, Show m, Show n) => Show (m :+: n) where
  showsPrec p c = showParen (p > 5) $
    showsPrec 11 m . showString " :+: " . showsPrec 11 n
    where (m,n) = untangle c

instance (Action m n, Semigroup m, Semigroup n) => Semigroup (m :+: n) where
  C n1 m1 o1 <> C n2 m2 o2 = C (n1 <> act' m1 (o1 <> n2)) (m1 <> m2) o2
  {-# INLINE (<>) #-}

instance (Action m n, Semigroup m, Semigroup n) => Monoid (m :+: n) where
  mempty  = C Nought Nought Nought
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

-- | Coproducts act on other things by having each of the components
--   act individually.
instance (Action m n, Action m r, Action n r, Semigroup n) => Action (m :+: n) r where
  act (C n m o) = act'' n' . act'' m
    where !n' = n <> act' m o
  {-# INLINE act #-}

-- | Construct a coproduct with a left value.
inL :: m -> m :+: n
inL m = C Nought (Only m) Nought
{-# INLINE inL #-}

-- | Construct a coproduct with a right value.
inR :: n -> m :+: n
inR r = C (Only r) Nought Nought
{-# INLINE inR #-}

-- | Prepend a value from the left.
prependL :: Semigroup m => m -> m :+: n -> m :+: n
prependL m' (C n m o) = C n (Only m' <> m) o
{-# INLINE prependL #-}

-- | Prepend a value from the right.
prependR :: Semigroup n => n -> m :+: n -> m :+: n
prependR n' (C n m o) = C (Only n' <> n) m o
{-# INLINE prependR #-}

-- | Extract @m@ from a coproduct.
killR :: Monoid m => m :+: n -> m
killR (C _ m _) = get m
{-# INLINE killR #-}

-- | Extract @n@ from a coproduct.
killL :: (Action m n, Monoid' n) => m :+: n -> n
killL (C n m o) = get $ n <> act' m o
{-# INLINE killL #-}

untangle :: (Action m n, Monoid m, Monoid' n) => m :+: n -> (m,n)
untangle (C n m o) = (get m, get n')
  where !n' = n <> act' m o
{-# INLINE untangle #-}

-- Lenses --------------------------------------------------------------

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Lens onto the both @m@ and @n@.
untangled :: (Action m n, Monoid m, Monoid' n) => Lens (m :+: n) (m' :+: n') (m,n) (m',n')
untangled f c = f (untangle c) <&> \(m',n') -> C (Only n') (Only m') Nought
{-# INLINE untangled #-}
-- this could be an iso if we depended on profunctors

-- | Lens onto the left value of a coproduct.
_L :: (Action m n, Monoid m, Semigroup n) => Lens (m :+: n) (m' :+: n) m m'
_L f (C n m o) = f (get m) <&> \m' -> C (n <> act' m o) (Only m') Nought
{-# INLINE _L #-}
-- this could be a prism if we depended on profunctors

-- | Lens onto the right value of a coproduct.
_R :: (Action m n, Monoid' n) => Lens (m :+: n) (m :+: n') n n'
_R f (C n m o) = f (get $ n `mappend` act' m o) <&> \n' -> C (Only n') m Nought
{-# INLINE _R #-}

-- Internal utilities --------------------------------------------------

get :: Monoid a => Possible a -> a
get (Only a) = a
get _        = mempty
{-# INLINE get #-}

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
{-# INLINE (<&>) #-}

-- Act on a possible with a possible
act' :: Action m n => Possible m -> Possible n -> Possible n
act' (Only m) (Only n) = Only (act m n)
act' _        n        = n
{-# INLINE act' #-}

-- Act with a possible
act'' :: Action m n => Possible m -> n -> n
act'' (Only m) = act m
act'' _        = id
{-# INLINE act'' #-}
