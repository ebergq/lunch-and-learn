import Polymorphism

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

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

