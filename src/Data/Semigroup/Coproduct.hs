{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase #-}

module Data.Semigroup.Coproduct
       ( (:+.)
       , inL, inR
       , cop
       , toAltList
       , toMonoid
       ) where

import Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Typeable (Typeable)
import Data.Semigroup (Endo(Endo, appEndo))
import Data.Semigroup.Foldable (foldMap1)

import Data.Monoid.Action (Action(..))
import Data.Monoid.Coproduct ((:+:))
import qualified Data.Monoid.Coproduct as M

-- | @m :+. n@ is the coproduct of semigroups @m@ and @n@.  Values of
--   type @m :+. n@ consist of alternating non-empty lists of @m@ and @n@
--   values. Composition is list concatenation, with appropriate
--   combining of adjacent elements
newtype m :+. n = SCo { unSCo :: NonEmpty (Either m n) }
  deriving (Typeable, Show)

instance (Eq m, Eq n, Semigroup m, Semigroup n) => Eq (m :+. n) where
  (==) = (==) `on` (normalize . unSCo)

-- | Extract a semigroup coproduct to a non-empty list of @Either@ values.
--   The resulting list is guaranteed to be normalized, in the sense that
--   it will strictly alternate between @Left@ and @Right@.
toAltList :: (Semigroup m, Semigroup n) => (m :+. n) -> NonEmpty (Either m n)
toAltList (SCo ms) = normalize ms

-- Normalize a list of @Either@ values by combining any consecutive
-- values of the same type.
normalize :: (Semigroup m, Semigroup n) => NonEmpty (Either m n) -> NonEmpty (Either m n)
normalize = \case
  Left e1 :| Left e2 : es -> normalize (Left (e1 <> e2) :| es)
  Right e1 :| Right e2 : es -> normalize (Right (e1 <> e2) :| es)
  e1 :| es1 -> case es1 of
    e2 : es2 -> (e1 :| []) <> normalize (e2 :| es2)
    [] -> e1 :| []

-- | Universal map of the coproduct. Both functions in the signature
--   should be semigroup homomorphisms. The name @cop@ is an abbreviation
--   for copairing.
cop :: Semigroup k => (m -> k) -> (n -> k) -> (m :+. n) -> k
f `cop` g = foldMap1 (either f g) . unSCo

-- | Injection from the left semigroup into a coproduct.
inL :: m -> m :+. n
inL m = SCo (Left m :| [])

-- | Injection from the right semigroup into a coproduct.
inR :: n -> m :+. n
inR n = SCo (Right n :| [])

-- | Given monoids @m@ and @n@, we can form their semigroup coproduct
--   @m :+. n@. Every monoid homomorphism is a semigroup homomorphism.
--   In particular the canonical inections of the monoid coproduct from
--   @m@ and @n@ into @m :+: n@ are semigroup homomorphisms. By pairing
--   them using the universal property of the semigroup coproduct we
--   obtain a canonical semigroup homomorphism `toMonoid` from @m :+. n@
--   to @m :+: n@.
toMonoid :: (Monoid m, Monoid n) => m :+. n -> m :+: n
toMonoid = M.inL `cop` M.inR

instance Semigroup (m :+. n) where
  (SCo es1) <> (SCo es2) = SCo (es1 <> es2)

-- | Coproducts act on other things by having each of the components
--   act individually.
instance (Action m r, Action n r) => Action (m :+. n) r where
  act = appEndo . ((Endo . act) `cop` (Endo . act))
