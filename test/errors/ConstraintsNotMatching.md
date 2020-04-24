# ConstraintsNotMatching
This error arises when some of the infered constraints don't match with the declared constraints.
Here the `ToInt` constraint was not specified.
```
class ToInt a = {
  sig toInt = a -> Int;
}
sig mkIntPlusOne = forall a . a -> Int;
let mkIntPlusOne a = toInt a + 1;
```
>>>TypeError : Infered constraints not found in definition : (ToInt 'a) at fileName 6:1
