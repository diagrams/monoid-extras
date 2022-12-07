{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
       , Regular(..)
       , Conjugate(..)
       , Torsor(..)
       ) where

import           Data.Semigroup
import           Data.Group

------------------------------------------------------------
--  Monoid and semigroup actions
------------------------------------------------------------

-- | Type class for monoid (and semigroup) actions, where monoidal
--   values of type @m@ \"act\" on values of another type @s@.
--   Instances are required to satisfy the laws
--
--   * @act mempty = id@
--
--   * @act (m1 \`mappend\` m2) = act m1 . act m2@
--
--   Semigroup instances are required to satisfy the second law but with
--   ('<>') instead of 'mappend'.  Additionally, if the type @s@ has
--   any algebraic structure, @act m@ should be a homomorphism.  For
--   example, if @s@ is also a monoid we should have @act m mempty =
--   mempty@ and @act m (s1 \`mappend\` s2) = (act m s1) \`mappend\`
--   (act m s2)@.
--
--   By default, @act = const id@, so for a type @M@ which should have
--   no action on anything, it suffices to write
--
--   > instance Action M s
--
--   with no method implementations.
--
--   It is a bit awkward dealing with instances of @Action@, since it
--   is a multi-parameter type class but we can't add any functional
--   dependencies---the relationship between monoids and the types on
--   which they act is truly many-to-many.  In practice, this library
--   has chosen to have instance selection for @Action@ driven by the
--   /first/ type parameter.  That is, you should never write an
--   instance of the form @Action m SomeType@ since it will overlap
--   with instances of the form @Action SomeMonoid t@.  Newtype
--   wrappers can be used to (awkwardly) get around this.
class Action m s where

  -- | Convert a value of type @m@ to an action on @s@ values.
  act :: m -> s -> s
  act = const id

-- | @()@ acts as the identity.
instance Action () l where
  act () = id

-- | @Nothing@ acts as the identity; @Just m@ acts as @m@.
instance Action m s => Action (Maybe m) s where
  act Nothing  s = s
  act (Just m) s = act m s

-- | @Endo@ acts by application.
--
--   Note that in order for this instance to satisfy the @Action@
--   laws, whenever the type @a@ has some sort of algebraic structure,
--   the type @Endo a@ must be considered to represent /homomorphisms/
--   (structure-preserving maps) on @a@, even though there is no way
--   to enforce this in the type system.  For example, if @a@ is an
--   instance of @Monoid@, then one should only use @Endo a@ values
--   @f@ with the property that @f mempty = mempty@ and @f (a <> b) =
--   f a <> f b@.
instance Action (Endo a) a where
  act = appEndo

instance Num a => Action Integer (Sum a) where
  n `act` a = fromInteger n <> a

instance Num a => Action Integer (Product a) where
  n `act` a = fromInteger n <> a

instance Fractional a => Action Rational (Sum a) where
  n `act` a = Sum (fromRational n) <> a

instance Fractional a => Action Rational (Product a) where
  n `act` a = Product (fromRational n) <> a

-- | An action of a group is "free transitive", "regular", or a "torsor"
--   iff it is invertible.
--
--   Given a value `s2` that has been acted upon by `m`,
--   and the original value `s1`, it is possible to recover the acting `m`.
--   This is encoded in the laws:
--
--   * @(m `'act'` s) `'difference'` s = m@
--   * @(s1 `'difference'` s2) `'act'` s2 = s1@
class Group m => Torsor m s where

  -- | @'difference' s1 s2@ is the element @m@ such that @s1 = m `'act'` s2@.
  difference :: s -> s -> m

-- | Any monoid acts on itself by left multiplication.
--   This newtype witnesses this action:
--   @'getRegular' $ 'Regular' m1 `'act'` 'Regular' m2 = m1 '<>' m2@
newtype Regular m = Regular { getRegular :: m }

instance Semigroup m => Action m (Regular m) where
  m1 `act` Regular m2 = Regular $ m1 <> m2

instance Group m => Torsor m (Regular m) where
  Regular m1 `difference` Regular m2 = m1 ~~ m2

-- | Any group acts on itself by conjugation.
newtype Conjugate m = Conjugate { getConjugate :: m }

instance Group m => Action m (Conjugate m) where
  m1 `act` Conjugate m2 = Conjugate $ m1 <> m2 ~~ m1
