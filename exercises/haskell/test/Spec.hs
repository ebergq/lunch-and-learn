import Polymorphism
import Monoids

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- Tests for Polymorphism
-------------------------

prop_Id1 :: Int -> Bool
prop_Id1 x = id' x == x

prop_Id2 :: Maybe Int -> Bool
prop_Id2 x = id'' x == x

prop_Fst :: Int -> String -> Bool
prop_Fst x y = fst' x y == x

prop_Snd :: Int -> String -> Bool
prop_Snd x y = snd' x y == y

prop_Head :: [Int] -> Property
prop_Head xs = xs /= [] ==> head' xs == head xs

prop_Last :: [Int] -> Property
prop_Last xs = xs /= [] ==> last' xs == last xs

prop_MaximumMono1 :: [Int] -> Property
prop_MaximumMono1 xs = xs /= [] ==> maximumMono xs `elem` xs

prop_MaximumMono2 :: [Int] -> Property
prop_MaximumMono2 xs =
    xs /= [] ==> forAll (choose (0, length xs - 1)) $ \n -> (maximumMono xs) >= (xs !! n)

prop_MaximumPoly :: [Int] -> Property
prop_MaximumPoly xs =
    xs /= [] ==> forAll (choose (0, length xs - 1)) $ \n -> (maximumPoly xs) >= (xs !! n)

prop_Map :: [Int] -> Bool
prop_Map xs = map' f xs == map f xs
  where
    f = (+1)

-- Tests for Monoids
--------------------

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

main :: IO ()
main = hspec $ do
  describe "Polymorphism" $ do
    it "prop_Id1" $ property prop_Id1
    it "prop_Fst" $ property prop_Fst
    it "prop_Snd" $ property prop_Snd
    prop "prop_Head" $ prop_Head
    prop "prop_Last" $ prop_Last
    prop "prop_MaximumMono1" prop_MaximumMono1
    prop "prop_MaximumMono2" prop_MaximumMono2
    prop "prop_MaximumPoly" prop_MaximumPoly
    it "prop_Id2" $ property prop_Id2
    it "prop_Map" $ property prop_Map
  describe "Monoids" $ do
    it "unit_SumIdentity" $ unit_SumIdentity
    it "prop_SumMonoid" $ property prop_SumMonoid
    it "prop_SumAssociative" $ property prop_SumAssociative
    it "prop_SumLeftIdenity" $ property prop_SumLeftIdentity
    it "prop_SumRightIdentity" $ property prop_SumRightIdentity
    it "unit_ProductIdentity" $ unit_ProductIdentity
    it "prop_ProductMonoid" $ property prop_ProductMonoid
    it "prop_ProductAssociative" $ property prop_ProductAssociative
    it "prop_ProductLeftIdenity" $ property prop_ProductLeftIdentity
    it "prop_ProductRightIdentity" $ property prop_ProductRightIdentity
    it "unit_AnyIdentity" $ unit_AnyIdentity
    it "prop_AnyAssociative" $ property prop_AnyAssociative
    it "prop_AnyLeftIdentity" $ property prop_AnyLeftIdentity
    it "prop_AnyRightIdentity" $ property prop_AnyRightIdentity
    it "unit_AllIdentity" $ unit_AllIdentity
    it "prop_AllAssociative" $ property prop_AllAssociative
    it "prop_AllLeftIdentity" $ property prop_AllLeftIdentity
    it "prop_AllRightIdentity" $ property prop_AllRightIdentity
    it "unit_ListIdentity" $ unit_ListIdentity
    it "prop_ListAssociative" $ property prop_ListAssociative
    it "prop_ListLeftIdentity" $ property prop_ListLeftIdentity
    it "prop_ListRightIdentity" $ property prop_ListRightIdentity

