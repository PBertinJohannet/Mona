# Depths

With different constraints at different depths. (todo check infered signature)
```
data C a =
  | CI = Int -> C Int; 
  | CA = a -> C a;
  | CC = C (C a) -> C (C a);
  | CR = C (f a) -> C (f a);
  | CCI = C (C Int) -> C (C Int);

data Nil = | Nil = Nil;

let CAintoA c = case c of
  (CA (CI i)) -> i,
  (CA (CA i)) -> i;

let CIntintoInt c = case c of
  (CA (CI i)) -> i,
  (CA (CA i)) -> i + 1;

```

## Correct CA

Here we just need to check the final signature.
```

let correct1 a = (CAintoA (CA a)) + 1;

let correct2 a = (CIntintoInt (CA (CA a))) + 1;

let main = printInt 4;


```
>>>compiled successfully

## Incorrect CA


Here we expect the wrong arg into CA
```
let incorrect = (CAintoA Nil);

let main = printInt 4;
```
>>>TypeError : Cannot unify : C (C 'a) with Nil at fileName 21:1 at fileName 21:26


