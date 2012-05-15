-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.PosInf
-- Copyright   :  (c) 2012 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Make a semigroup under 'min' into a monoid by adjoining an element
-- corresponding to positive infinity.
--
-----------------------------------------------------------------------------

module Data.Monoid.PosInf
       ( PosInf(..)
       , minimum
       ) where

import           Data.Semigroup
import qualified Prelude as P
import           Prelude hiding (minimum)

data PosInf a = Finite a | PosInfty
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (PosInf a) where
  (<>) = min

instance Ord a => Monoid (PosInf a) where
  mempty = PosInfty
  mappend = (<>)

minimum :: Ord a => [a] -> PosInf a
minimum xs = P.minimum (PosInfty : map Finite xs)