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

## Should not work

When it has nothing to do with the input it should not
```
let andmoremaybe = assign andMore Star;
let main = printInt (fromStar andmoremaybe);
```
>>>TypeError : Cannot unify : (Star -> Star) -> Star with Star at fileName 18:1 at fileName 18:35
