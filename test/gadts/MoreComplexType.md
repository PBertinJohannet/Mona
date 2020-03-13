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

When it has nothing to do with the input it should not
```

let fail a = case a of
    (RI i) -> True,
    (RB b) -> 1;
``` 
>>>TypeError : Could not reconcile : R Int 'c -> Bool and R Bool 'f -> Int at fileName 16:1
