-- | A strict version of the semi-direct product. If a monoid m acts
--   on s then this version of the semi-direct product is strict in
--   the m-portion of the semi-direct product.

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Data.Monoid.SemiDirectProduct.Strict
       ( Semi, quotient, inject, embed
       ) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
#endif

import           Data.Monoid.Action

-- | The semi-direct product of monoids @s@ and @m@, which is a monoid
--   when @m@ acts on @s@. Structurally, the semi-direct product is
--   just a pair @(s,m)@.  However, the monoid instance is different.
--   In particular, we have
--
-- > (s1,m1) <> (s2,m2) = (s1 <> (m1 `act` s2), m1 <> m2)
--
--   We call the monoid @m@ the quotient monoid and the monoid @s@ the
--   sub-monoid of the semi-direct product. The semi-direct product
--   @Semi s m@ is an extension of the monoid @s@ with @m@ being the
--   quotient.
data Semi s m = Semi s !m


instance (Monoid m, Monoid s, Action m s) => Monoid (Semi s m) where
  mempty                            = Semi mempty mempty
  {-# INLINE mempty #-}
  mappend (Semi xs xm) (Semi ys ym) = Semi (xs `mappend` (xm `act` ys)) (xm `mappend` ym)
  {-# INLINE mappend #-}
  mconcat                           = foldr mappend mempty
  {-# INLINE mconcat #-}

-- | The quotient map.
quotient :: Semi s m -> m
quotient (Semi _ m) = m

-- | The injection map.
inject :: Monoid m => s -> Semi s m
inject = flip Semi mempty

-- | The semi-direct product gives a split extension of @s@ by
--   @m@. This allows us to embed @m@ into the semi-direct
--   product. This is the embedding map. The quotient and embed maps
--   should satisfy the equation @quotient . embed = id@.
embed :: Monoid s => m -> Semi s m
embed = Semi mempty
