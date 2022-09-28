# Safe head
```
data NonEmpty = | NonEmpty = NonEmpty;
data Empty = | Empty = Empty;

data List x y =
  | Nil = List Empty a;
  | Cons = a -> List b a -> List NonEmpty a;

let safeHead = \(Cons a b) -> a;
```
## noErr
```
let main = printInt (safeHead (Cons 2 Nil));
```
>>>compiled successfully
*safeHead:forall a . [NonEmpty] a -> a

## Err
```
let main = printInt (safeHead Nil);
```
>>>TypeError : Cannot unify : NonEmpty with Empty at fileName 12:1 at fileName 12:31
