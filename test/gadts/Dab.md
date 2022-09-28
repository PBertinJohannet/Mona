# Dab

Given this not simple type 
```

let main = printInt 4;

data Bool = 
    | True = Bool;
    | False = Bool;

data D a b c = 
  | CI = Int -> Int -> Bool ->  D Int Int Int;
  | CB = Bool -> Int -> Bool -> D Bool Int Bool;
  | CA = a -> b -> c -> D a b c;

```

## should fail correctly


todo
```


let correct f = \a -> case a of
  (CI i j k) -> CB k j k,
  (CB i j k) -> CB k j k,
  (CA i j k) -> CA k j k;
```
>>>TypeError : Found no matching substitution for 'b -> 'aa at fileName 18:1


## plus dur que 2 plus haut

Here it should fail because the first branch forces a Bool and the third requires an Int as arg param.
```
let incorrect f = \a -> case a of
  (CA i j (False)) -> CA False i True,
  (CA i j k) -> CA k i k,
  (CI i j k) -> CB k j k,
  (CB i j k) -> CB k j k;
```
>>>TypeError : Wrong branch Type, cand : Int final : Bool at fileName 16:1

## enfin un correct

todo
```
let correct f = \a -> case a of
  (CA i j k) -> CA j j j,
  (CI i j k) -> CA j j j,
  (CB i j k) -> CA j j j;
```
>>>compiled successfully
*correct:forall a b c d . a -> D b c d -> D c c c

## trouver exemple uncollapsables

todo
```
let main = printInt 4;
```
>>>compiled successfully
*main:IO Unit
