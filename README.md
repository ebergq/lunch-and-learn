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
  * [Different kinds of type](#different-kinds-of-types)
* [Monoids](#monoids)
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

##### Why use purely functional languages?
 - Immutability
 - Purity
 - Separation of side effects
 - Easier domain modeling
 - Concurrent/Parallell programming
 - Terse, easier to solve problems with less code

Functional programming is like Lego. Using type signatures you can build programs by composing functions without implementing the actual functions. And who doesn't like Lego?

##### Why learn more advanced topics in functional programming?
For the same reason why you should learn the SOLID principles in object-oriented programming. Knowledge about advanced concepts is needed to understand and build better architecture using functional programming. There are patterns in OOP which are not easy to understand, like Adapter, Mediator, SOLID, Singleton, Visitor, etc.

Parametric Polymorphism
=======================

##### What is parametric polymorphism?
A function working on many different types. One implementation to do a thing which can be re-used by many different types. Much more useful in languages where purity is enforced (**Haskell**, **Elm**, **PureScript**). The less we know about the type, the more we know about the implementation. The more we know about the type, the less we know about the implementation.

##### Canonical example

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

##### More examples

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

##### The `maximum` function

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

##### Different kinds of type

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

TBC

Functors
========

TBC

Applicative functors
====================

TBC

Monads
======

TBC

Exercises
=========

##### Install Stack

This project is written as a Haskell project using [Stack](https://docs.haskellstack.org/). To install it, follow the installation instructions for your platform on the home page.

##### Build

To build the project use the following command:
```sh
$ stack build
```

##### Run tests

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
