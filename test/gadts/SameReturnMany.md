# Same return

Given this simple type 
```

let main = printInt 4;

data R a = 
    | RI = Int -> R Int;
    | RC = Char -> R Char;
    | RA = a -> R a;
    | RB = Bool -> R Bool;

```

## Should work

When the return is the same, it should compile correctly
```

let correct a = case a of
    (RC i) -> 1,
    (RI i) -> i,
    (RA i) -> 2,
    (RB b) -> 0;

```
>>>compiled successfully

## Should also work

When the return can be determined from the input, it should compile correctly
```

let correct a = case a of
    (RC i) -> i,
    (RI i) -> i,
    (RA i) -> i,
    (RB b) -> b;

```
>>>compiled successfully

## Should not work

When it has nothing to do with the input it should not
```
data Something = | S = Something;

let fail a = case a of
    (RC i) -> 1,
    (RI i) -> i,
    (RA i) -> 2,
    (RB b) -> S;
```
>>>TypeError : could not generalize the types : Int Int Int Something at fileName 15:1 at fileName 15:14
