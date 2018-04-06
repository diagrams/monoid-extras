{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Data.Monoid.SemiDirectProduct
       ( Semi, unSemi, tag, inject, untag, embed, quotient
       ) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid        (Monoid(..))
#endif
import           Data.Semigroup     (Semigroup(..))

import           Data.Monoid.Action

-- | The semi-direct product of monoids @s@ and @m@, which is a monoid
--   when @m@ acts on @s@. Structurally, the semi-direct product is
--   just a pair @(s,m)@.  However, the monoid instance is different.
--   In particular, we have
--
-- > (s1,m1) <> (s2,m2) = (s1 <> (m1 `act` s2), m1 <> m2)
--
--   We think of the @m@ values as a "tag" decorating the @s@ values,
--   which also affect the way the @s@ values combine.
--
--   We call the monoid @m@ the quotient monoid and the monoid @s@ the
--   sub-monoid of the semi-direct product. The semi-direct product
--   @Semi s m@ is an extension of the monoid @s@ with @m@ being the
--   quotient.
newtype Semi s m = Semi { unSemi :: (s,m) }

instance (Semigroup m, Semigroup s, Action m s) => Semigroup (Semi s m) where
  x <> y = Semi (xs <> (xm `act` ys), xm <> ym)
    where (xs, xm) = unSemi x
          (ys, ym) = unSemi y
  {-# INLINE (<>) #-}

#if MIN_VERSION_base(4,8,0)
  sconcat = foldr1 (<>)
  {-# INLINE sconcat #-}
#endif

instance (Monoid m, Monoid s, Action m s) => Monoid (Semi s m) where
  mempty      = Semi (mempty, mempty)
  {-# INLINE mempty #-}

#if !MIN_VERSION_base(4,11,0)
  mappend x y = Semi (xs `mappend` (xm `act` ys), xm `mappend` ym)
    where (xs, xm) = unSemi x
          (ys, ym) = unSemi y

  {-# INLINE mappend #-}
#endif

  mconcat     = foldr mappend mempty
  {-# INLINE mconcat #-}

-- | Tag an @s@ value with an @m@ value to create an element of the
--   semi-direct product.
tag :: s -> m -> Semi s m
tag s m = Semi (s,m)

-- | The injection map, /i.e./ give an @s@ value a trivial tag.
inject :: Monoid m => s -> Semi s m
inject = Semi . (,mempty)

-- | Forget the monoidal tag.  Of course, @untag . inject = id@, and
--   @untag (tag s m) = s@.
untag :: Semi s m -> s
untag = fst . unSemi

-- | Embed a "tag" value as a value of type @Semi s m@.  Note that
--
--   @inject s <> embed m = tag s m@
--
--   and
--
--   @embed m <> inject s@ = tag (act m s) m@
--
--   The semi-direct product gives a split extension of @s@ by
--   @m@. This allows us to embed @m@ into the semi-direct
--   product. This is the embedding map. The quotient and embed maps
--   should satisfy the equation @quotient . embed = id@.
embed :: Monoid s => m -> Semi s m
embed = Semi . (mempty,)

-- | The quotient map, /i.e./ retrieve the monoidal tag value.
quotient :: Semi s m -> m
quotient = snd . unSemi


