{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.WithSemigroup
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenience alias for the combination of @Monoid@ and @Semigroup@ constraints.
--
-----------------------------------------------------------------------------

module Data.Monoid.WithSemigroup
       ( Monoid'
       ) where

import           Data.Semigroup

-- Poor man's constraint synonym.  Eventually, once it becomes
-- standard, we can make this a real constraint synonym and get rid of
-- the UndecidableInstances flag.  Better yet, hopefully the Monoid
-- class will eventually have a Semigroup superclass.

-- | The @Monoid'@ class is a synonym for things which are instances
--   of both 'Semigroup' and 'Monoid'.  Ideally, the 'Monoid' class
--   itself will eventually include a 'Semigroup' superclass and we
--   can get rid of this.
class (Semigroup m, Monoid m) => Monoid' m
instance (Semigroup m, Monoid m) => Monoid' m
