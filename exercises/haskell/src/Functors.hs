{-# LANGUAGE InstanceSigs #-}
module Functors where

import Prelude hiding (Either(..), Identity(..), Maybe(..))

data Identity a = Identity a
  deriving (Eq, Show)

data Maybe a = Nothing | Just a
  deriving (Eq, Show)

data Either e a = Left e | Right a
  deriving (Eq, Show)

-- Implement the functor instance for Identity.
-- Hint: Pattern match on the Identity instance.
instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f identity = error "Functor for Identity is not implemented"

-- Implement the functor instance for Maybe.
-- Hint: Pattern match on the Maybe instance.
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f maybe = error "Functor for Maybe is not implemented"

-- Implement the functor instance for Either (notice that the first type
-- parameter is partially applied which makes Either a functor on the second
-- type parameter only).
-- Hint: Pattern match on the Either instance.
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap f either = error "Functor for Either is not implemented"
