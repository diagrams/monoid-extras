{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE MagicHash          #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Monoid.Set where

import Data.Data
import Data.Semigroup
import Data.Foldable
import Data.Traversable

import GHC.Exts (isTrue#, dataToTag#)
import Unsafe.Coerce (unsafeCoerce)

----

-- | @Set@ is like @Maybe@, but the value can either be
--   unspecified with @Unset@, or explicitly cleared with @Clear@
data Set a
   = Unset
   | Set a
   | Clear
   deriving (Data, Typeable, Show, Read, Functor, Foldable, Traversable)


-- | The right-hand-side or "newer" value is prefered, unless it
--   is Unset, in which case the old value is left unchanged
instance Semigroup (Set a) where

  l <> Unset = l
  _ <> r     = r

  stimes = stimesIdempotentMonoid

instance Monoid (Set a) where
  mempty = Unset


isSet :: Set a -> Bool
isSet s = isTrue# (dataToTag# s)

maybeToSet :: Maybe a -> Set a
maybeToSet = unsafeCoerce

setToMaybe :: Set a -> Maybe a
setToMaybe (Set a) = Just a
setToMaybe _       = Nothing

