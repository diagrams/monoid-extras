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
       ) where

import           Data.Semigroup

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
instance Action m s => Action (Option m) s where
  act (Option Nothing)  s = s
  act (Option (Just m)) s = act m s

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

