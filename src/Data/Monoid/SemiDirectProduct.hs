{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Data.Monoid.SemiDirectProduct
       ( Semi, quotient, inject, embed
       ) where

import Data.Monoid
import Data.Monoid.Action

-- | The semi-direct product of monoids @s@ and @m@. When the monoid
-- @m@ acts on the monoid @s@, this type acquires a monoid structure.
-- We call the monoid @m@ the quotient monoid and the monoid @s@ the
-- sub-monoid of the semi-direct product. The semi-direct product
-- @Semi s m@ is an extension of the monoid @s@ with @m@ being the
-- quotient.
newtype Semi s m = Semi { unSemi :: (s,m) }


instance (Monoid m, Monoid s, Action m s) => Monoid (Semi s m) where
  mempty      = Semi (mempty, mempty)
  mappend x y = Semi (xs `mappend` (xm `act` ys), xm `mappend` ym)
    where (xs, xm) = unSemi x
          (ys, ym) = unSemi y


-- | The quotient map.
quotient :: Semi s m -> m
quotient = snd . unSemi

-- | The injection map.
inject :: Monoid m => s -> Semi s m
inject = Semi . (,mempty)

-- | The semi-direct product gives a split extension of @s@ by
-- @m@. This allows us to embed @m@ into the semi-direct product. This
-- is the embedding map. The quotient and embed maps should satisfy
-- the equation @quotient . embed = id@.
embed :: Monoid s => m -> Semi s m
embed = Semi . (mempty,)
