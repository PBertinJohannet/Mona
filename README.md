# The Mona programming language

Mona is a purely functional programming language featuring :

* Strong static Typing
* Hindley Milner Type system
* Type inference
* Type classes
* Higher kinded types
* Kind inference
* Kind polymorphism
* Pattern matching
* Generalized Algebraic Data Types
* A very small set of native functions
* Bugs (not too much hopefully)

# Examples

## Defining functions

Function definitions start with `let`, You can declare arguments on the left of the equal or use the lambda syntax.

```
let add3Sim a b c = a + b + c;
let add3Lam = \a b c -> a + b + c;
```

## Branching

Branching is done through if-then-else and pattern matching, the first being just sugar for the second. The clauses of the patterns are lazily evaluated.

```
let fac a = if (a == 0) then a else a * (fac (a - 1));

let isNone a = case a of
  (Just k) -> False,
  (Nothing) -> True;
```

## Explicit typing

You can specify function's types using the `sig` keyword. It is especially necessary for recursion because the name of the function is not known at compile time.

```
sig fac = Int -> Int;
let fac a = if (a == 0) then a else a * (fac (a - 1));
```

## Fix

If you want to use recursion in lambdas you can use the `fix` keyword.

```
let fac = fix (\f a -> if a == 0 then a else a (f (a - 1)));
```

## GADT Syntax

Data declarations uses a similar syntax than Haskell's GADTs.

You can, for example use it to create a safe head function.
```

data NonEmpty = | NonEmpty = NonEmpty;
data Empty = | Empty = Empty;

data List x y =
  | Nil = List Empty a;
  | Cons = a -> List b a -> List NonEmpty a;

sig safeHead = forall x . (List NonEmpty x) -> x;
let safeHead = \(Cons a b) -> a;

let main = printInt (safeHead (Cons 2 Nil));

```

This code will then fail to compile :

```
let main = printInt (safeHead Nil);
```

## TypeClasses

You can declare type classes using the `class` keyword.

```
class Functor f = {
  sig fmap = forall a b . (a -> b) -> (f a) -> f b;
}
```

And instance with the `inst` keyword.

```
inst Maybe of Functor = {
  let fmap f v = case v of
    (Just j) -> Just (f j),
    (Nothing) -> Nothing;
}
```

## Printing to the terminal.

Prints 7.

```
let main = printInt 7;
```

## No prelude.

Mona does not support importing modules yet (but it will soon I promise) and the if-then-else relies on the existence of the `Bool` datatype. In the same way, the `print` function relies on the existence of the `Print` class. `flip` is also needed so you have to include them at the top of the file.

```
sig flip = forall a b c . (a -> b -> c) -> b -> a -> c;
let flip = \f a b -> (f b) a;

data Bool =
  | True = Bool;
  | False = Bool;

class Print s = {
  sig print = s -> List (IO Unit);
}
```

## Complete example

Folding Natural numbers to Ints using a catamorphism on the Maybe functor :

```
data Fix f = | Fix = f (Fix f) -> Fix f;

sig unfix = forall f . (Fix f) -> f (Fix f);
let unfix = \(Fix a) -> a;

class Functor f = {
  sig fmap = forall a b . (a -> b) -> (f a) -> f b;
}

data Maybe a =
  | Just = a -> Maybe a
  | Nothing = Maybe a;
data Nat = | Nat = Fix Maybe -> Nat;
let S = \(Nat i) -> Nat (Fix (Just i));
let Z = Nat (Fix (Nothing));

inst Maybe of Functor = {
  let fmap f v = case v of
    (Just j) -> Just (f j),
    (Nothing) -> Nothing;
}

sig cata = forall a f . Functor f => ((f a) -> a) -> (Fix f) -> a
let cata alg a = alg ((fmap (cata alg)) (unfix a));

sig toInt = Nat -> Int;
let toInt = \(Nat i) -> cata (\a -> case a of
  (Just k) -> k + 1,
  (Nothing) -> 0 ) i;

let four = S (S (S (S Z)));

let main = printInt (toInt four);
```
prints four

# Changelog

## 17/10/2019
 - Changed data syntax to GADT
 - Added support for polymorphic kinds
 - Arrows associate to the left in type signatures
 - Run time Error messages carry a little bit more information

# TODO
 - Import modules
 - Better error messages
 - compilation


# Acknowledgments

[These](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf) [two](http://dev.stephendiehl.com/fun/006_hindley_milner.html) have helped me a lot making type inference work for mona.
