# catamorphism

This is the example of the readme.
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
