# The Mona programming language

Mona is a purely functional programming language featuring :

* Strong static Typing
* Hindley Milner Type system
* Type inference
* Type classes
* Higher kinded types
* Kind inference
* Pattern matching
* Sum and Product types
* A very small set of native functions
* Bugs (not too much hopefully)

# Examples

## Function

Adding 3 numbers.

```
let add3 a b c = a + b + c;
```

## Lambda

Same thing.

```
let add3 = \a b c -> a + b + c;
```

## Condition

Basic if-then-else condition.
The clauses are lazily evaluated, allowing this example to produce a value in finite time.

```
let fac a = if (a == 0) then a else a * (fac (a - 1));
```

## Explicit typing

Actually the previous example wont compile because fac is not defined. You have to give it a signature first.

```
sig fac = Int -> Int;
let fac a = if (a == 0) then 1 else a * (fac (a - 1));
```

## Fixed point combinator

Or you can use fix if you really don't want to write the signature.

```
let facf f a = if a == 0 then 1 else a (f (a - 1));
let fac = fix facf;
```

## Declaring a type

There is only one way of declaring a type, similar to the data declaration in haskell.

```
data Maybe a = Just a | Nothing;
```

## Fixing a type

Mona does not support recursion in types. You have to use Fix.

```
data Nat = Nat (Fix Maybe);
```

## Patterns

Once you have created a type you can pattern match on it, it is lazily evaluated like the if.

```
sig isNone = Maybe Int -> Bool;
let toIntA a = case a of
  (Just k) -> False,
  (Nothing) -> True;
```

## TypeClasses

The famous functor.

```
class Functor f = {
  sig fmap = forall a b . (a -> b) -> (f a) -> f b;
}
```

With an instance.

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

Mona does not support importing modules so you have to copy the basic prelude in your file.
The interpreter needs some of these definitions to be exactly like that so don't try to change it.

```sig flip = forall a b c . (a -> b -> c) -> b -> a -> c;
let flip = \f a b -> (f b) a;

data Fix = Unit;
data Fix f = Fixed (f (Fix f));
data Fix f = Fixed (f (Fix f));
sig unfix = forall f . (Fix f) -> f (Fix f);
let unfix = \(Fixed a) -> a;

data Unit = Unit;
data Bool = True | False;

class Functor f = {
  sig fmap = forall a b . (a -> b) -> (f a) -> f b;
}

class Print s = {
  sig print = s -> List (IO Unit);
}

inst List Int of Print = {
  let print = fmap printInt;
}

data ListF f a = EndF | ConsF a (f a);
data List a  = List ((fix ListF) a);
let Cons = (\a b -> List(ConsF a b));
let End = List(EndF);

inst List of Functor = {
  let fmap f = \(List l) -> case l of
    (EndF) -> End,
    (ConsF a b) -> Cons (f a) (fmap f b);
}

```

## Complete example

Folding Natural numbers to Ints using a catamorphism on the Maybe functor :

```
data Maybe a = Just a | Nothing;
data Nat = Nat (Fix Maybe);
let S = \(Nat i) -> Nat (Fixed (Just i));
let Z = Nat (Fixed (Nothing));

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

# Acknowledgments

[These](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf) [two](http://dev.stephendiehl.com/fun/006_hindley_milner.html) have helped me a lot making type inference work for mona.
