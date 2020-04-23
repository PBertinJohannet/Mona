# Same return

Given this simple type 
```

let main = printInt 4;

data Bool = 
    | True = Bool;
    | False = Bool;

data Prod a b = | P = a -> b -> Prod a b;
data Sum a b = 
    | Left = a -> Sum a b;
    | Right = b -> Sum a b;

data R a = 
    | RP = P a a -> R a;
    | RS = S a a -> R a;

```

## Should work

When the return is the same, it should compile correctly
```

let correct a = case a of
    (RP (P a b)) -> a,
    (RS s) -> case s of
        (Left a) -> a,
        (Right b) -> b;

```
>>>compiled successfully

## Should also work

When the return can be determined from the input, it should compile correctly
```

let correct a = case a of
    (RP i) -> 1,
    (RS b) -> 0;

```
>>>compiled successfully

## Should work with same sum

When it has nothing to do with the input it should not
```

let fail a = case a of
    (RS i) -> Left True,
    (RP b) -> Left 1;
```
>>>TypeError : could not generalize the types : Bool Int at fileName 21:1 at fileName 21:14
