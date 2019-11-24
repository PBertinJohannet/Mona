# CheckStrict

To verify that the signature is correctly checked with the definition.
```
let main = printInt 5;
```

## Simple specialization

```
sig desInt = forall a . (Int -> Int) -> a -> Int
let desInt f a = (f 5);
```
>>>compiled successfully

## Refused

```
sig wrong = forall a . (a -> a) -> a -> a;
let wrong f a = (f a) + 5;
```
>>>TypeError : Cannot unify : Int with 'a at fileName 6:1
