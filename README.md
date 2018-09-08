Lunch and Learn
===============

This is an attempt to teach some advanced functional programming principles using examples in Haskell. Much of the content is inspired by the excellent [LambdaCast](https://soundcloud.com/lambda-cast) podcast.

Table of Contents
=================

* [Introduction](#introduction)
  * [Why use purely functional languages?](#why-use-purely-functional-languages)
  * [Why learn more advanced topics in functional programming?](#why-learn-more-advanced-topics-in-functional-programming)
* [Parametric Polymorphism](#parametric-polymorphism)
  * [What is parametric polymorphism?](#what-is-parametric-polymorphism)
  * [Canonical example](#canonical-example)
  * [More examples](#more-examples)
  * [The `maximum` function](#the-maximum-function)
  * [Different kinds of type](#different-kinds-of-type)
* [Monoids](#monoids)
  * [Definition of Monoid](#definition-of-monoid)
  * [Monoid laws](#monoid-laws)
  * [Monoid examples](#monoid-examples)
* [Functors](#functors)
* [Applicative functors](#applicative-functors)
* [Monads](#monads)
* [Exercises](#exercises)
  * [Install Stack](#install-stack)
  * [Build](#build)
  * [Run tests](#run-tests)
* [References](#references)

Introduction
============

#### Why use purely functional languages?
 - Immutability
 - Purity
 - Separation of side effects
 - Easier domain modeling
 - Concurrent/Parallell programming
 - Terse, easier to solve problems with less code

Functional programming is like Lego. Using type signatures you can build programs by composing functions without implementing the actual functions. And who doesn't like Lego?

#### Why learn more advanced topics in functional programming?
For the same reason why you should learn the SOLID principles in object-oriented programming. Knowledge about advanced concepts is needed to understand and build better architecture using functional programming. There are patterns in OOP which are not easy to understand, like Adapter, Mediator, SOLID, Singleton, Visitor, etc.

Parametric Polymorphism
=======================

#### What is parametric polymorphism?
A function working on many different types. One implementation to do a thing which can be re-used by many different types. Much more useful in languages where purity is enforced (**Haskell**, **Elm**, **PureScript**). The less we know about the type, the more we know about the implementation. The more we know about the type, the less we know about the implementation.

#### Canonical example

How many implementations are there for this example?

```hs
f1 :: a -> a
f1 x = ?
```

```cs
// Note: No default(T), reflection (mutation, side effects) or similar.
public static T F1<T>(T x) => ?;
```

What about this one?

```hs
f2 :: Int -> Int
f2 x = ?
```

```cs
public static int F2(int x) => ?;
```

Over-specification is a problem. It's kind of like pre-mature optimization where you commit to a specific type. The number of implementations of a function escalates quickly which makes it hard to reason about the code. *John A De Goes* talks about this in his [blog](http://degoes.net/articles/insufficiently-polymorphic) on the subject.

> Descriptive variable names are a code smell.
> More precisely, if you can name your variables after more descriptive things than `f`, `a`, `b`, and so on, then your code is probably monomorphic.
> Monomorphic code is much more likely to be incorrect than polymorphic code, because for every type signature, there are many more possible implementations.

#### More examples

How many implementations exist for the following functions?
```hs
f3 :: a -> b -> a
f3 x y = ?
```

```cs
public static T1 F3<T1, T2>(T1 x, T2 y) => ?;
```

---

```hs
f4 :: a -> a -> a
f4 x y = ?
```

```cs
public static T F4<T>(T x, T y) => ?;
```

---

```hs
f5 :: [a] -> a
f5 xs = ?
```

```cs
public static T F5<T>(IEnumerable<T> xs) => ?;
```

#### The `maximum` function

Consider the following function:
```hs
maximum :: [Int] -> Int
maximum xs = ?
```

To be able to be sure this function behaves correctly, we need two property-based tests:
```hs
prop_MaximumResultInOriginalList :: [Int] -> Property
prop_MaximumResultInOriginalList xs = xs /= [] ==> maximum xs `elem` xs

prop_MaximumAllElementsSmallerThanMaximum :: [Int] -> Property
prop_MaximumAllElementsSmallerThanMaximum xs =
    xs /= [] ==> forAll (choose (0, length xs - 1)) $ \n -> maximum xs >= (xs !! n)
```

What happens if we generalize `maximum`?
```hs
maximum :: Ord a => [a] -> a
maximum xs = ?
```

Now we know that if the function returns a result it must have come from the original list. This makes the property `prop_MaximumResultInOriginalList` unnecessary since, we get this certainty by the type system at compile time. Another added benefit is that `maximum` can be used with any type that is an instance of the `Ord` type class. So, by generalizing functions like this one, we get a lot of added benefits and no drawbacks!

#### Different kinds of type

So far we have touched on the following kinds of type (also visualized in the table below):
 - Concrete types
 - Polymorphic types
 - Concrete types parameterized by concrete types
 - Concrete types parameterized by polymorphic types

| Type | Concrete examples | Polymorphic examples |
| ---- | ----------------- | -------------------- |
| Simple type | `Int`, `Double`, `String` | `a`, `b`, `c` |
| Container type | `[Int]`, `[a]`, `Map k v` | `?` |

But what about the `?` in the table above? A polymorphic type parameterized by another polymorphic type. This class of types are called *higher-kinded types* and can only be expressed in a few languages like **Haskell**, **PureScript**, **Scala**, **Idris** etc. **C\#**, **F\#**, **Elm** and similar languages can not express this.

What can we say about this function?
```hs
f6 :: f a -> f a
f6 x = ?
```

If we replace `f` with a concrete type like `[]` we get the following type signature:
```hs
f7 :: [a] -> [a]
f7 xs = ?
```

What about constraints? Constraints on the `a` in `f a` is interesting, but constraints on the `f` is where we can utilize very powerful abstractions. Consider if we had the following constraint on the `f`:
```hs
f8 :: (a -> b) -> f a -> f b
```

If we replace the `f` with `IEnumerable`, it will look like this in **C\#**:
```cs
public static IEnumerable<T2> F8<T1, T2>(this IEnumerable<T1> source, Func<T1, T2> f) => ?;
```

This is `Select` in **C\#** and `map` (or `fmap` in **Haskell**) in other languages. This mapping function can be implemented for different container types:
- `List.map :: (a -> b) -> [a] -> [b]`
- `Maybe.map :: (a -> b) -> Maybe a -> Maybe b`

`map` can also be implemented for container types with multiple type arguments if we partially apply all but one of the type arguments, for example:
- `Map.map :: (a -> b) -> Map k a -> Map k b`
- `Tuple.map :: (b -> c) -> (a, b) -> (a, c)`

We often call the higher-kinded type variable `f` or `m` because of the concepts *Functor* and *Monad*.

What do we know about the mapping function? It the provided function is run it must be run on elements extracted from `f a` since we have no way of constructing values of type `a`.

Monoids
=======

#### Definition of Monoid

A Monoid is a classification of types that have an associative binary operator, often called `append` (or `mappend` in **Haskell** for historical reasons and name clashes), and an *empty* element, often called `empty` (or `mempty` in **Haskell**).

This is expressed by a type class in **Haskell**:
```hs
class Monoid a where
  mappend :: a -> a -> a
  mempty :: a
  mconcat :: [a] -> a
  {-# MINIMAL mempty, mappend #-}
```
Notice that there is a function `mconcat` in there which does not have to be implemented. It is a convenience function for combining a list of elements into a single element.

In **C\#** this can be implemented by an interface:
```cs
public interface Monoid<T>
{
    public T Empty { get; }
    public T Append(T other);
}
```

The fact that *append* has to be associative means it doesn't matter if we append from the left or from the right first. This makes *Divide and Conquer* and parallell computing strategies viable for doing the appending operations.

#### Monoid laws

##### Associativity

The associative law states that the binary operator must be associative:
```hs
(a `mappend` b) `mappend` c == a `mappend` (b `mappend` c)
```

---

```cs
(a.Append(b)).Append(c) == a.Append(b.Append(c))
```

##### Left identity

The left identity law states that when the identity element (`mempty`) is appended to another element from the left, nothing happens:

```hs
mempty `mappend` a == a
```

---

```cs
Empty.Append(a) == a
```

##### Right identity

The right identity law states that when the identity element (`mempty`) is appended to another element from the right, nothing happens:

```hs
a `mappend` mempty == a
```

---

```cs
a.Append(Empty) == a
```

#### Monoid examples

##### Sum

For integers, we can define two different Monoids, one for addition and one for multiplication. We then say that *`Int` under addition is a Monoid* or *`Int` under multiplication is a Monoid*. We can not say that `Int` is a Monoid since, there is ambiguity about what Monoid to use.

In **Haskell** we can define the following wrapper for a *Sum* type:
```hs
newtype Sum a = Sum a
```

This type can be made a Monoid by:
```hs
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)
```

With this implementation, what does the following code return?
```hs
mconcat (map Sum [1, 2, 3, 4, 5]) = ?
```

##### Product

The implementation of Monoid for a product type is left as an exercise.

##### Any

Like `Sum`, we can implement an additive Monoid for booleans:
```hs
newtype Any = Any Bool

instance Monoid Any where
  mempty = Any False
  mappend (Any a) (Any b) = Any (a || b)
```

##### All

The implementation of Monoid for type `All` is left as an exercise.

##### List

Monoid for list is pretty straightforward:
```hs
data List a = Nil | Cons a (List a)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x (xs `mappend` ys)
```

##### Functions

Functions with type `a -> a` can be made a Monoid where the identity function `id` is the empty element and function composition `(.)` is the append function:

```hs
newtype Function a = Function (a -> a)

instance Monoid (Function a) where
  mempty = Function id
  mappend (Function f) (Function g) = Function (f . g)
```

##### Logging

Monoids can also be viewed as an accumulating state where you can add more stuff to it which makes it useful when thinking about logging.

```hs
newtype Log a = Log [a]

instance Monoid (Log a) where
  mempty = Log []
  mappend (Log xs) (Log ys) = Log (xs ++ ys)
```

This is of course a boring example when only working with lists in-memory but when combined with Monads and IO this becomes very powerful. If the append function also is *commutative*, meaning `mappend a b == mappend b a`, it becomes even more powerful since, we do not have to care about the order of the arguments. More comprehensive examples can be found on [Dan Piponis blog](http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html).

Functors
========

#### Definition of Functor

A Functor is a classification of types that have a mapping function, often called `map` (or `fmap` in **Haskell**).

This is expressed by a type class in **Haskell**:
```hs
class  Functor f  where
  fmap        :: (a -> b) -> f a -> f b
  (<$)        :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```
Notice that there is an infix operator `(<$)` in there which does not have to be implemented. It replaces all values in the input with the same value. There are also other operators to which can work with a functor such as `($>) :: Functor f => a -> f b -> f a` and `(<$>) :: Functor f => (a -> b) -> f a -> f b`. `($>)` is the flipped version of `(<$)` and `(<$>)` is the infix version of `fmap`.

Since **C\#** does not have higher-kinded types this can not be implemented for the general case but if it could it would probably look something along the lines of:
```cs
public interface Functor<F<A>>
{
    public F<B> Map<B>(Func<A, B>);
}
```

However, there is a non-general implementation for `IEnumerable<T>` implemented as an extension method in .Net:
```cs
public static class EnumerableExtensions
{
    ...
    public static IEnumerable<B> Select<A, B>(this IEnumerable<A> source, Func<A, B);
    ...
}
```

#### Functor laws

##### Identity

The identity law states that if you map the identity function over the functor, it is the same as the result of using the identity function on the functor:
```hs
fmap id == id
```

---

```cs
source.Select(x => x) == source
```

##### Composition

The composition law states that two consecutive mapping operations, `f` and `g`, is the same as one mapping operation where the mapping operation is the composed function `h = f . g`:

```hs
fmap (f . g) == fmap f . fmap g
```

---

```cs
source.Select(x => F(x)).Select(x => G(x)) == source.Select(x => G(F(x))
```

#### Functor examples

##### Identity

**Identity** is a wrapping structure in **Haskell** and can be implemented as:
```hs
newtype Identity a = Identity a
```

**Identity** can implement the Functor type class as:
```hs
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
```

The proofs for the functor laws are trivial:
```hs
-- Identity, LHS
  fmap id (Identity x)                [Definition of fmap]
= Identity (id x)                     [Definition of id]
= Identity x

-- Identity, RHS
  id (Identity x)                     [Definition of id]
= Identity x

-- Composition, LHS
  fmap (f . g) (Identity x)           [Definition of fmap]
= Identity ((f . g) x)                [Definition of (.)]
= Identity (f (g x))

-- Composition, RHS
  (fmap f . fmap g) (Identity x)      [Definition of (.)]
= fmap f (fmap g (Identity x))        [Definition of fmap]
= fmap f (Identity (g x))             [Definition of fmap]
= Identity (f (g x))
```

##### Maybe

**Maybe** is a structure which is either empty (`Nothing`) or containing a value (`Just x`). It is defined in **Haskell** as:
```hs
data Maybe a = Nothing | Just a
```

Functor for **Maybe** is implemented as:
```hs
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

Proofs that the implementation of Functor for **Maybe** abides the Functor laws are left as an exercise.

##### Either

**Either** is a structure containing one of two values: `Right x` or `Left y`. It is defined in **Haskell** as:
```hs
data Either e a = Left e | Right a
```

Functor for **Either e** (notice that we have to partially apply the first type argument to `Either`) is implemented as:
```hs
instance Functor (Either e) where
  fmap f (Left y)  = Left y
  fmap f (Right x) = Right (f x)
```

Proofs that the implementation of Functor for **Either e** abides the Functor laws are left as an exercise.

Applicative functors
====================

TBC

Monads
======

TBC

Exercises
=========

#### Install Stack

This project is written as a Haskell project using [Stack](https://docs.haskellstack.org/). To install it, follow the installation instructions for your platform on the home page.

#### Build

To build the project use the following command:
```sh
$ stack build
```

#### Run tests

To run the tests use the follow command:
```sh
$ stack test
```

To run specific tests or fixtures a filter can be used in the command:
```sh
$ stack test --test-arguments "-m <filter>"
```

References
==========

 * [LambdaCast](https://soundcloud.com/lambda-cast/)
 * [Haskell home page](https://wiki.haskell.org/)
 * [Learn You A Haskell](http://learnyouahaskell.com/)
 * [Blog: John A De Goes](http://degoes.net/articles/)
 * [Blog: Dan Piponi](http://blog.sigfpe.com/)
