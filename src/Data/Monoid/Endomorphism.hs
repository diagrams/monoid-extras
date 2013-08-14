module Data.Monoid.Endomorphism where

import Control.Category
import Data.Semigroup
import Data.Semigroupoid
import Prelude hiding ((.), id)


-- | 
-- An 'Endomorphism' in a given 'Category' is a morphism from some object to itself.
newtype Endomorphism k a = Endomorphism {getEndomorphism :: k a a}

instance Semigroupoid k => Semigroup (Endomorphism k a) where
  Endomorphism a <> Endomorphism b = Endomorphism (a `o` b)

instance Category k => Monoid (Endomorphism k a) where
  mempty = Endomorphism id
  Endomorphism a `mappend` Endomorphism b = Endomorphism (a . b)