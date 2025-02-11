*This is an unfinished project, some tests still don't pass*

# Mona

Mona is a project programming language with full type inference for GADTs,

The only constraint is that there must not be an expression with two case expressions using gadts, the type must be specified for at least one.

# Examples

More complex examples are available there:
https://github.com/PBertinJohannet/Mona/blob/master/test/gadts/ArgCount.md

## SafeList 

Using GADTs to make a safe ```head``` function
```
data NonEmpty = | NonEmpty = NonEmpty;
data Empty = | Empty = Empty;

data List x y =
  | Nil = List Empty a;
  | Cons = a -> List b a -> List NonEmpty a;

let safeHead = \(Cons a b) -> a;
```
Now ```safeHead``` will always return a result of the correct type.

## Simple example

Infers the type of the correct function and refuses the incorrect one (explanation there: https://github.com/PBertinJohannet/Mona/blob/master/test/gadts/MoreComplexType.md).
```

let main = printInt 4;

data Bool = 
    | True = Bool;
    | False = Bool;

data R a b= 
    | RI = a -> R Int a;
    | RB = a -> R Bool a;

let correct_1 a = case a of
    (RI i) -> 1,
    (RB b) -> True;

let correct_2 a = case a of
    (RI i) -> 1,
    (RB b) -> 0;


let BToInt b = case b of (True) -> 1, (False) -> 4;

let incorrect a = case a of
    (RI i) -> 1 + i,
    (RB b) -> BToInt b;

```


## Folding Natural numbers to Ints using a catamorphism on the Maybe functor :

Nothing to do with gadts but shows the language correctly infers and execute "normal" types

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
Prints four ()

## More examples 

The folder ```test``` contains easly readable files (in markdown format) and examples that can be run with ```stack test```


