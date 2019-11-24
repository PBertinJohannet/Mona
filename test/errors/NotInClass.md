# NotInClass
This errror is thrown when a variable does not satisfy a constraint.
Let's say we have a wrapper for an int.
```
data IntWrap = | Int = Int -> IntWrap;
class Compare a = {
  sig getMax = a -> a -> Int;
};
let main = printInt (getMax (Int 2) (Int 3));
```

## Without class implementation
If we try to compare values of type `IntWrap` without implementing anything we get an error because it does not implements the `Compare` constraint.
```
```
>>>TypeError : IntWrap is not in Compare at fileName 6:1 at fileName 6:22

## With implementation
We need to implement the `Compare` class for the `IntWrap` type.
```
inst IntWrap of Compare = {
  let getMax = \(Int a) (Int b) -> 0;
}
```
>>>compiled successfully
