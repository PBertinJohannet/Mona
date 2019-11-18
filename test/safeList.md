# Safe head
```
data NonEmpty = | NonEmpty = NonEmpty;
data Empty = | Empty = Empty;

data List x y =
  | Nil = List Empty a;
  | Cons = a -> List b a -> List NonEmpty a;

sig safeHead = forall x . (List NonEmpty x) -> x;
let safeHead = \(Cons a b) -> a;
```
## noErr
```
let main = printInt (safeHead (Cons 2 Nil));
```
>>> ok

## Err
```
let main = printInt (safeHead Nil);
```
>>> ohohoh
