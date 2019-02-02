{-# LANGUAGE InstanceSigs #-}
module Demo where

-- Maybe is already defined in Haskell Prelude module
-- so hide it to be able to re-implement it here.
import Prelude hiding (Maybe(..))

-- Datatype
data Maybe a = Nothing | Just a
  deriving (Eq, Show)

-- Make it a Functor instance
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = undefined

{-
Verifying the functor laws:
- Hint: Use definition of:
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

id :: a -> a
id x = x

- Identity:       fmap id = id

- Composition:    fmap (f . g) = fmap f . fmap g

-}

-- Make it a Monad instance
instance Monad Maybe where
  return :: a -> Maybe a
  return = undefined

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) = undefined

-- Example
division :: Int -> Int -> Maybe Int
division a b = case b of
  0 -> Nothing
  _ -> Just (a `div` b)

{-
Verifying the monad laws:

- Left identity:       return a  >>= f      = f a

- Right identity:      m         >>= return = m

- Associativity:       (m >>= f) >>= g      = m >>= (\x -> f x >>= g)

-}









{- The type must also be an Applicative to be able to be a monad -}
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = undefined

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = undefined
