# IntWrap
Wrapper for an int.
```
data IntWrap = | Int = Int -> IntWrap;
```

## With compare
```
sig compare = IntWrap -> IntWrap -> Int
let compare = \(Int a) (Int b) -> a;
let main = printInt (compare (Int 2) (Int 3));
```
>>>compiled successfully

## With const
```
sig getMax = IntWrap -> IntWrap -> Int;
let getMax = \(Int a) -> const;
let main = printInt (getMax (Int 2) (Int 3));
```

### polymorphic const
```
sig const = forall a . a -> Int;
let const b = 0;
```
>>>compiled successfully

### Specialized const
```
sig const = IntWrap -> Int;
let const b = 0;
```
>>>compiled successfully
