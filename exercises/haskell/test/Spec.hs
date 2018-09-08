import qualified MonoidSpec as Monoid
import qualified PolymorphismSpec as Polymorphism

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Polymorphism" $ do
    it "prop_Id1" $ property Polymorphism.prop_Id1
    it "prop_Fst" $ property Polymorphism.prop_Fst
    it "prop_Snd" $ property Polymorphism.prop_Snd
    prop "prop_Head" $ Polymorphism.prop_Head
    prop "prop_Last" $ Polymorphism.prop_Last
    prop "prop_MaximumMono1" Polymorphism.prop_MaximumMono1
    prop "prop_MaximumMono2" Polymorphism.prop_MaximumMono2
    prop "prop_MaximumPoly" Polymorphism.prop_MaximumPoly
    it "prop_Id2" $ property Polymorphism.prop_Id2
    it "prop_Map" $ property Polymorphism.prop_Map
  describe "Monoids" $ do
    it "unit_SumIdentity" $ Monoid.unit_SumIdentity
    it "prop_SumMonoid" $ property Monoid.prop_SumMonoid
    it "prop_SumAssociative" $ property Monoid.prop_SumAssociative
    it "prop_SumLeftIdenity" $ property Monoid.prop_SumLeftIdentity
    it "prop_SumRightIdentity" $ property Monoid.prop_SumRightIdentity
    it "unit_ProductIdentity" $ Monoid.unit_ProductIdentity
    it "prop_ProductMonoid" $ property Monoid.prop_ProductMonoid
    it "prop_ProductAssociative" $ property Monoid.prop_ProductAssociative
    it "prop_ProductLeftIdenity" $ property Monoid.prop_ProductLeftIdentity
    it "prop_ProductRightIdentity" $ property Monoid.prop_ProductRightIdentity
    it "unit_AnyIdentity" $ Monoid.unit_AnyIdentity
    it "prop_AnyAssociative" $ property Monoid.prop_AnyAssociative
    it "prop_AnyLeftIdentity" $ property Monoid.prop_AnyLeftIdentity
    it "prop_AnyRightIdentity" $ property Monoid.prop_AnyRightIdentity
    it "unit_AllIdentity" $ Monoid.unit_AllIdentity
    it "prop_AllAssociative" $ property Monoid.prop_AllAssociative
    it "prop_AllLeftIdentity" $ property Monoid.prop_AllLeftIdentity
    it "prop_AllRightIdentity" $ property Monoid.prop_AllRightIdentity
    it "unit_ListIdentity" $ Monoid.unit_ListIdentity
    it "prop_ListAssociative" $ property Monoid.prop_ListAssociative
    it "prop_ListLeftIdentity" $ property Monoid.prop_ListLeftIdentity
    it "prop_ListRightIdentity" $ property Monoid.prop_ListRightIdentity
