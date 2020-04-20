# Same return

Given this simple type 
```

let main = printInt 4;

data R a = 
    | RI = Int -> R Int;
    | RB = Bool -> R Bool;

```

## Should work

When the return is the same, it should compile correctly
```

let correct a = case a of
    (RI i) -> i,
    (RB b) -> 0;

```
>>>compiled successfully

## Should work

When the return depends on the input, it should compile correctly
```

let correct a = case a of
    (RI i) -> i,
    (RB b) -> b;

```
>>>compiled successfully

## Should not work

When it has nothing to do with the input it should not
```
data Something = | S = Something;

let fail a = case a of
    (RI i) -> i,
    (RB b) -> S;
```
>>>TypeError : could not generalize the types : Int Something at fileName 13:1
