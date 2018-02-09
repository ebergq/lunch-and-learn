module Monoids where

newtype Sum a = Sum a
  deriving (Eq, Show)

newtype Product a = Product a
  deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum $ x + y

{-
Implement a Monoid instance for the Product type.
-}
instance Num a => Monoid (Product a) where
  mempty = undefined
  mappend = undefined

newtype Any = Any Bool
  deriving (Eq, Show)

newtype All = All Bool
  deriving (Eq, Show)

instance Monoid Any where
  mempty = Any False
  mappend (Any a) (Any b) = Any $ a || b

instance Monoid All where
  mempty = undefined
  mappend = undefined

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

{-
Implement a Monoid instance for List.
Hint: Use pattern matching and recursion for mappend.
-}
instance Monoid (List a) where
  mempty = undefined
  mappend = undefined

