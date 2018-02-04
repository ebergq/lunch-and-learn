module Polymorphism where

{-
Since many of these functions are standard functions which already exist
':s are added to the name to distinguish from the original one.
-}

id' :: a -> a
id' x = undefined

fst' :: a -> b -> a
fst' x y = undefined

{-
There is obvious solution to this but can you solve this using point-free
notation?
Hint: Use the following functions:
 - (.) :: (b -> c) -> (a -> b) -> a -> c
 - flip :: (a -> b -> c) -> b -> a -> c
 - fst' :: a -> b -> a
-}
snd' :: a -> b -> b
snd' x y = undefined

{-
Implement a function which retrieves the first element in a list.
Hint: Use pattern matching:
 - []     - Empty list
 - (x:xs) - First element x with xs as the rest of the list
-}
head' :: [a] -> a
head' xs = undefined

{-
Implement a function which retrieves the last element in a list.
-}
last' :: [a] -> a
last' xs = undefined

maximumMono :: [Int] -> Int
maximumMono [] = error "maximumMono: empty list"
maximumMono (x:xs) = f x xs
  where
    f acc [] = acc
    f acc (x:xs) | x > acc = f x xs
                 | otherwise = f acc xs

{-
Implement the polymorphic version of maximum
Hint: Use compare from the Ord type class
-}
maximumPoly :: Ord a => [a] -> a
maximumPoly xs = undefined

id'' :: f a -> f a
id'' x = undefined

