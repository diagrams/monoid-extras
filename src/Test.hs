{-# LANGUAGE StandaloneDeriving #-}

import Test.QuickCheck
import Control.Applicative

import Data.Monoid.Cut
import Data.Semigroup

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Arbitrary a => Arbitrary (Cut a) where
  arbitrary = oneof [ Uncut <$> arbitrary
                    , liftA2 (:||:) arbitrary arbitrary
                    ]

deriving instance Eq a => Eq (Cut a)

type S = Sum Int

prop_idL :: Cut S -> Bool
prop_idL c = mempty <> c == c

prop_idR :: Cut S -> Bool
prop_idR c = c <> mempty == c

prop_mappend_assoc :: Cut S -> Cut S -> Cut S -> Bool
prop_mappend_assoc a b c = (a <> b) <> c == a <> (b <> c)