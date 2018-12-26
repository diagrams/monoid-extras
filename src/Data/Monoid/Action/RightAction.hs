{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Action.RightAction
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Monoid and semigroup right actions.
--
-----------------------------------------------------------------------------

module Data.Monoid.Action.RightAction
       ( RightAction(..)
       ) where

import           Data.Semigroup

------------------------------------------------------------
--  Monoid and semigroup right actions
------------------------------------------------------------

-- | Type class for right monoid (and semigroup) actions, where monoidal
--   values of type @m@ act (@rightAct@) on values of another type @s@.
--   Instances are required to satisfy the laws
--
--   * @mempty \`rightAct\` s = s@
--
--   * @s \`rightAct\` (m1 \`mappend\` m2) = (s `rightAct` m1) `rightAct` m2@
--
--   Semigroup instances are required to satisfy the second law but with
--   ('<>') instead of 'mappend'.  Additionally, if the type @s@ has
--   any algebraic structure, @(\`rightAct\` m)@ should be a homomorphism.  For
--   example, if @s@ is also a monoid we should have @rightAct mempty m =
--   mempty@ and @rightAct (s1 \`mappend\` s2) m = (rightAct s1 m) \`mappend\`
--   (rightAct s2 m)@.
--
--   By default, @rightAct = const@, so for a type @M@ which should have
--   no action on anything, it suffices to write
--
--   > instance RightAction M s
--
--   with no method implementations.
--
--   It is a bit awkward dealing with instances of @RightAction@, since it
--   is a multi-parameter type class but we can't add any functional
--   dependencies---the relationship between monoids and the types on
--   which they act is truly many-to-many.  In practice, this library
--   has chosen to have instance selection for @RightAction@ driven by the
--   /first/ type parameter.  That is, you should never write an
--   instance of the form @RightAction m SomeType@ since it will overlap
--   with instances of the form @Action SomeMonoid t@.  Newtype
--   wrappers can be used to (awkwardly) get around this.
class RightAction m s where

  -- | Convert a value of type @m@ to an right action on @s@ values.
  rightAct :: s -> m -> s
  rightAct = const

-- | @()@ acts as the identity.
instance RightAction () l where
  rightAct m () = m

-- | @Nothing@ acts as the identity; @Just m@ acts as @m@.
instance RightAction m s => RightAction (Option m) s where
  rightAct s (Option Nothing)  = s
  rightAct s (Option (Just m)) = rightAct s m
