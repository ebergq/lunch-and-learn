module FunctorSpec where

import Prelude hiding (Either(..), Identity(..), Maybe(..))
import Test.QuickCheck

import Functors

prop_IdentityLaw :: (Eq (f a), Functor f) => f a -> Bool
prop_IdentityLaw x = fmap id x == id x

prop_CompositionLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
prop_CompositionLaw f g x = fmap (f . g) x == (fmap f . fmap g) x

-- Testing the Identity type
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

prop_IdentityTypeUpholdsIdenityLaw :: Identity Int -> Bool
prop_IdentityTypeUpholdsIdenityLaw = prop_IdentityLaw

prop_IdentityTypeUpholdsCompositionLaw :: Int -> Identity Int -> Bool
prop_IdentityTypeUpholdsCompositionLaw n = prop_CompositionLaw (+n) (*n)

-- Testing the Maybe type
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, pure Nothing), (2, pure Just <*> arbitrary)]

prop_MaybeTypeUpholdsIdentityLaw :: Maybe Int -> Bool
prop_MaybeTypeUpholdsIdentityLaw = prop_IdentityLaw

prop_MaybeTypeUpholdsCompositionLaw :: Int -> Maybe Int -> Bool
prop_MaybeTypeUpholdsCompositionLaw n = prop_CompositionLaw (+n) (*n)

-- Testing the Either type
instance (Arbitrary e, Arbitrary a) => Arbitrary (Either e a) where
  arbitrary = frequency [(1, pure Left <*> arbitrary), (1, pure Right <*> arbitrary)]

prop_EitherTypeUpholdsIdentityLaw :: Either String Int -> Bool
prop_EitherTypeUpholdsIdentityLaw = prop_IdentityLaw

prop_EitherTypeUpholdsCompositionLaw :: Int -> Identity Int -> Bool
prop_EitherTypeUpholdsCompositionLaw n = prop_CompositionLaw (+n) (*n)
