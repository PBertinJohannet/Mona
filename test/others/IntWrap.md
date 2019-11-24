# IntWrap
Wrapper for an int.
```
data IntWrap = | Int = Int -> IntWrap;
class Compare a = {
  sig compare = forall a . a -> a -> Bool;
};
```

# With constZ
```
sig constZ = IntWrap -> Int;
let constZ b = 0;
sig getMax = IntWrap -> IntWrap -> Int;
let getMax = \(Int a) -> constZ;
let main = printInt (getMax (Int 2) (Int 3));
```

# With compare
```
instance cpo
let main = printInt (getMax (Int 2) (Int 3));
```
