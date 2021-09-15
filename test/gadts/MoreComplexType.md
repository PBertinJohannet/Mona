# Same return

Given this simple type 
```

let main = printInt 4;

data Bool = 
    | True = Bool;
    | False = Bool;

data R a b= 
    | RI = a -> R Int a;
    | RB = a -> R Bool a;

```

## Should work

When the return is the same, it should compile correctly
```

let correct a = case a of
    (RI i) -> 1,
    (RB b) -> 0;

```
>>>compiled successfully

## Should also work

When the return can be determined from the input, it should compile correctly
```

let correct a = case a of
    (RI i) -> 1,
    (RB b) -> True;

```
>>>compiled successfully

## More complex example

When the return can be determined from the input, it should compile correctly
```

let correct a = case a of
    (RI i) -> 1 + i,
    (RB b) -> case b of (True) -> 1, (False) -> 4;

```
>>>compiled successfully

## Should not work

When it has nothing to do with neither the input or the other output it should not
```

let fail a = case a of
    (RI i) -> True,
    (RB b) -> 1;
``` 
>>>TypeError : could not generalize the types : Bool Int at fileName 16:1 at fileName 16:14

## Should refuse

refuse coz subpattern C a b does not cover all type possibles (a != C a b)
```
data Bool =
  | True = Bool
  | False = Bool;

data Prod a b = 
  | P = a -> b -> Prod a b;

data D a b = 
  | CI = Int -> Int -> D Int Int
  | CB = Bool -> Bool -> D Bool Bool
  | CA = a -> b -> D a b

let correct a = case a of
  (CI i j) -> P i j,
  (CA (CI i j) k) -> P (CI 1 2) 3,
  (CA (CB i j) k) -> P (CB True False) 3,
  (CB b j) -> P b 0;

```

## Should smh

```

data D a b = 
  | CI = Int -> Int -> D Int Int;
  | CB = Bool -> Bool -> D Bool Bool;
  | CR = k a b -> c -> D (k a b)c;
  | CA = a -> c -> D a c;
  | CD = D a b -> c -> D (D a b)c;

sig branch = forall a b . a -> a -> a;
let branch b c = if True then b else c;

sig useUnder = forall f a b . f a a b -> Bool;
let useUnder a = True;

sig useUnder1 = forall f b . f b -> Bool;
let useUnder1 a = True;

```


## fail 

let correct f = \a -> case a of
  (CI i j) -> CA 0 j,
  (CB i j) -> CA 1 j,
  (CA k l) -> CI k l;

## fail 


let correct f = \a -> case a of
  (CI i j) -> CA 0 j,
  (CB i j) -> CA 1 j,
  (CA k l) -> CI 5 l;