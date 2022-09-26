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

let CCAintoA c = case c of
  (CA (CI i)) -> i,
  (CA (CA i)) -> i;

let CCIntintoInt c = case c of
  (CA (CI i)) -> i,
  (CA (CA i)) -> i + 1;

```

## CAintoA

cover all cases and return the inside
```
let CAintoA c = case c of
  (CI i) -> i,
  (CA a) -> a,
  (CC a) -> a,
  (CR r) -> r,
  (CCI cci) -> cci;

let main = printInt 4;
```
>>>compiled successfully
*CAintoA:forall a . C a -> a


## CAintoInt

returns an int if inside, else returns the default
```
let CAintoIntf f i c = case c of
  (CI a) -> a,
  (CA a) -> a,
  (CC a) -> f i a,
  (CR r) -> f i r,
  (CCI cci) -> f cci;

let main = printInt 4;
```
>>>compiled successfully
*CAintoIntf:forall a . Int -> C a -> Int

## this one to
let CAintoA c = case c of
  (CC (CA a)) -> a;

## CCAintoA

check that CCAintoA has the right type
```
let main = printInt 4;
```
>>>compiled successfully
*CCAintoA:forall a . C (C a) -> a

## CCIntintoInt
check that CCIntintoInt has the right type
```
let main = printInt 4;
```
>>>compiled successfully
*CCIntintoInt:C (C Int) -> Int

## Correct CA

Here we just need to check the final signature.
```

let correct1 a = (CCAintoA (CA a)) + 1;

let correct2 a = (CCIntintoInt (CA (CA a))) + 1;

let main = printInt 4;


```
>>>compiled successfully
*correct1:C Int -> Int
*correct2:Int -> Int

## Incorrect CA


Here we expect the wrong arg into CA
```
let incorrect = (CCAintoA Nil);

let main = printInt 4;
```

>>>TypeError : Cannot unify : C (C 'a) with Nil at fileName 23:1 at fileName 23:27

