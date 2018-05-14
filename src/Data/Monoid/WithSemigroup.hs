{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
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

-- | For base < 4.11, the @Monoid'@ constraint is a synonym for things
--   which are instances of both 'Semigroup' and 'Monoid'.  For base
--   version 4.11 and onwards, @Monoid@ has @Semigroup@ as a
--   superclass already, so for backwards compatibility @Monoid'@ is
--   provided as a synonym for @Monoid@.
#if MIN_VERSION_base(4,11,0)
type Monoid' = Monoid
#else
type Monoid' m = (Semigroup m, Monoid m)
#endif
