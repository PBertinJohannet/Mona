# Polymorphic Kinds

Just to check it actually works

```
data Assign a b = | Assigned = f a -> Assign f a;
data Maybe a =
  | Just = a -> Maybe a;
  | Nothing = Maybe a;
let main = printInt 5;
```

## This should work
```
data MaybeIntA = | MIA = Assign Maybe Int -> MaybeIntA;
```
>>> ok

## This should not
```
data MaybeIntA = | MIA = Assign Int Int -> MaybeIntA;
```
>>> nok
