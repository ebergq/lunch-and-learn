module PolymorphismSpec where

import Test.QuickCheck

import Polymorphism

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
