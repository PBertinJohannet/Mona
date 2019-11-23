# UnificationFail

This error is usually thrown when a variable is of the wrong type or when a declaration does not match a signature.
```
let main = printInt 4;
```

## Variable of the wrong type
Here the variable a of type `Int` is added to a variable of type `IO Unit` but the signature of `(+)` is `Int -> Int -> Int`
```
let wrong a = main + a;
```
>>>TypeError : Cannot unify : Int with IO Unit at fileName 5:1 at fileName 7:1

## Function declaration does not match definition
Here the signatures says that the function should return an `Int` but it returns a variable of type `forall a . a`
```
sig retInt = forall a . a -> a -> Int;
let retInt a b = a;
```
>>>TypeError : Cannot unify : 'b with Int at fileName 6:1
