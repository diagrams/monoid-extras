-- | A strict version of the semi-direct product. If a monoid m acts
-- on s then this version of the semi-direct product is strict in the
-- m-portion of the semi-direct product.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE CPP                   #-}

module Data.Monoid.SemiDirectProduct.Strict
       ( Semi, quotient, inject, embed
       ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Data.Monoid.Action

-- | The semi-direct product of monoids @s@ and @m@. When the monoid
-- @m@ acts on the monoid @s@, this type acquires a monoid structure.
-- We call the monoid @m@ the quotient monoid and the monoid @s@ the
-- sub-monoid of the semi-direct product. The semi-direct product
-- @Semi s m@ is an extension of the monoid @s@ with @m@ being the
-- quotient.
data Semi s m = Semi s !m


instance (Monoid m, Monoid s, Action m s) => Monoid (Semi s m) where
  mempty                            = Semi mempty mempty
  mappend (Semi xs xm) (Semi ys ym) = Semi (xs `mappend` (xm `act` ys)) (xm `mappend` ym)

-- | The quotient map.
quotient :: Semi s m -> m
quotient (Semi s m) = m

-- | The injection map.
inject :: Monoid m => s -> Semi s m
inject = flip Semi mempty

-- | The semi-direct product gives a split extension of @s@ by
-- @m@. This allows us to embed @m@ into the semi-direct product. This
-- is the embedding map. The quotient and embed maps should satisfy
-- the equation @quotient . embed = id@.
embed :: Monoid s => m -> Semi s m
embed = Semi mempty
