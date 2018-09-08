module MonoidSpec where

import Test.QuickCheck

import Monoids

prop_Associative :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_Associative x y z = (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)

prop_LeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_LeftIdentity x = mempty `mappend` x == x

prop_RightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_RightIdentity x = x `mappend` mempty == x

-- Testing the Sum type
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary

unit_SumIdentity :: Bool
unit_SumIdentity = (mempty :: Sum Int) == Sum 0

prop_SumMonoid :: Sum Int -> Sum Int -> Bool
prop_SumMonoid (a@(Sum x)) (b@(Sum y)) = a `mappend` b == Sum (x + y)

prop_SumAssociative :: Sum Int -> Sum Int -> Sum Int -> Bool
prop_SumAssociative = prop_Associative

prop_SumLeftIdentity :: Sum Int -> Bool
prop_SumLeftIdentity = prop_LeftIdentity

prop_SumRightIdentity :: Sum Int -> Bool
prop_SumRightIdentity = prop_RightIdentity

-- Testing the Product type
instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = fmap Product arbitrary

unit_ProductIdentity :: Bool
unit_ProductIdentity = (mempty :: Product Int) == Product 1

prop_ProductMonoid :: Product Int -> Product Int -> Bool
prop_ProductMonoid (a@(Product x)) (b@(Product y)) = a `mappend` b == Product (x * y)

prop_ProductAssociative :: Product Int -> Product Int -> Product Int -> Bool
prop_ProductAssociative = prop_Associative

prop_ProductLeftIdentity :: Product Int -> Bool
prop_ProductLeftIdentity = prop_LeftIdentity

prop_ProductRightIdentity :: Product Int -> Bool
prop_ProductRightIdentity = prop_RightIdentity

-- Testing the Any type
instance Arbitrary Any where
  arbitrary = fmap Any arbitrary

unit_AnyIdentity :: Bool
unit_AnyIdentity = (mempty :: Any) == Any False

prop_AnyAssociative :: Any -> Any -> Any -> Bool
prop_AnyAssociative = prop_Associative

prop_AnyLeftIdentity :: Any -> Bool
prop_AnyLeftIdentity = prop_LeftIdentity

prop_AnyRightIdentity :: Any -> Bool
prop_AnyRightIdentity = prop_RightIdentity

-- Testing the All type
instance Arbitrary All where
  arbitrary = fmap All arbitrary

unit_AllIdentity :: Bool
unit_AllIdentity = (mempty :: All) == All True

prop_AllAssociative :: All -> All -> All -> Bool
prop_AllAssociative = prop_Associative

prop_AllLeftIdentity :: All -> Bool
prop_AllLeftIdentity = prop_LeftIdentity

prop_AllRightIdentity :: All -> Bool
prop_AllRightIdentity = prop_RightIdentity

-- Testing the List type
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (4, pure Cons <*> arbitrary <*> arbitrary)]

unit_ListIdentity :: Bool
unit_ListIdentity = (mempty :: List Int) == Nil

prop_ListAssociative :: List Int -> List Int -> List Int -> Bool
prop_ListAssociative = prop_Associative

prop_ListLeftIdentity :: List Int -> Bool
prop_ListLeftIdentity = prop_LeftIdentity

prop_ListRightIdentity :: List Int -> Bool
prop_ListRightIdentity = prop_RightIdentity
