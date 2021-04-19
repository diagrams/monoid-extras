{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Action.LeftAction
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Monoid and semigroup left actions.
--
-----------------------------------------------------------------------------

module Data.Monoid.Action.LeftAction
       ( LeftAction(..)
       ) where

import           Data.Semigroup

------------------------------------------------------------
--  Monoid and semigroup left actions
------------------------------------------------------------

-- | Type class for left monoid (and semigroup) actions, where monoidal
--   values of type @m@ act (@leftAct@) on values of another type @s@.
--   Instances are required to satisfy the laws
--
--   * @leftAct mempty = id@
--
--   * @leftAct (m1 \`mappend\` m2) = leftAct m1 . leftAct m2@
--
--   Semigroup instances are required to satisfy the second law but with
--   ('<>') instead of 'mappend'.  Additionally, if the type @s@ has
--   any algebraic structure, @leftAct m@ should be a homomorphism.  For
--   example, if @s@ is also a monoid we should have @leftAct m mempty =
--   mempty@ and @leftAct m (s1 \`mappend\` s2) = (leftAct m s1) \`mappend\`
--   (leftAct m s2)@.
--
--   By default, @leftAct = const id@, so for a type @M@ which should have
--   no action on anything, it suffices to write
--
--   > instance LeftAction M s
--
--   with no method implementations.
--
--   It is a bit awkward dealing with instances of @LeftAction@, since it
--   is a multi-parameter type class but we can't add any functional
--   dependencies---the relationship between monoids and the types on
--   which they act is truly many-to-many.  In practice, this library
--   has chosen to have instance selection for @LeftAction@ driven by the
--   /first/ type parameter.  That is, you should never write an
--   instance of the form @LeftAction m SomeType@ since it will overlap
--   with instances of the form @Action SomeMonoid t@.  Newtype
--   wrappers can be used to (awkwardly) get around this.
class LeftAction m s where

  -- | Convert a value of type @m@ to an left action on @s@ values.
  leftAct :: m -> s -> s
  leftAct = const id

-- | @()@ acts as the identity.
instance LeftAction () l where
  leftAct () = id

-- | @Nothing@ acts as the identity; @Just m@ acts as @m@.
instance LeftAction m s => LeftAction (Option m) s where
  leftAct (Option Nothing)  s = s
  leftAct (Option (Just m)) s = leftAct m s

-- | @Endo@ acts by application.
--
--   Note that in order for this instance to satisfy the @LeftAction@
--   laws, whenever the type @a@ has some sort of algebraic structure,
--   the type @Endo a@ must be considered to represent /homomorphisms/
--   (structure-preserving maps) on @a@, even though there is no way
--   to enforce this in the type system.  For example, if @a@ is an
--   instance of @Monoid@, then one should only use @Endo a@ values
--   @f@ with the property that @f mempty = mempty@ and @f (a <> b) =
--   f a <> f b@.
instance LeftAction (Endo a) a where
  leftAct = appEndo

