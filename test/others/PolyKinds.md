# Polymorphic Kinds

Just to check it actually works

```
data Assign a b = | Assigned = f a -> Assign f a;
data Maybe a =
  | Just = a -> Maybe a;
  | Nothing = Maybe a;
data More f = | M = f Int -> More f;
let main = printInt 5;
```

## This should work
```
data MaybeIntA = | MIA = Assign Maybe Int -> MaybeIntA;
```
>>>compiled successfully

## This should not
```
data MaybeIntA = | MIA = Assign Int Int -> MaybeIntA;
```
>>>DataDeclError : (KindUnificationFail) Could not unify kinds 'e -> * and  *

## More complex example
The `More` type has kind `(* -> *) -> *`.
```
data MoreMaybe = | MM = Assign More Maybe -> MoreMaybe;
```
>>>compiled successfully

## More complex fail
Here `AndMore` has kind `((* -> *) -> *) -> *`
```
data AndMore f = | M = f More -> AndMore f;
data AndMoreMaybe = | MM = Assign AndMore Maybe -> AndMoreMaybe;
```
>>>DataDeclError : (KindUnificationFail) Could not unify kinds (* -> *) -> * and  *
