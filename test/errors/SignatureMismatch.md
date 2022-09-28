# SignatureMismatch

This error arise when unifying a function declaration and its signature declaration if a variable is mapped to different types.
Here for example `a` and `b` are declared to be different but the declaration's type infers `b` to be the return type which is also `a`.
For more examples check `others/SignatureMismatch.md`
```
sig const = forall a b . a -> b -> a;
let const a b = b;
```
>>>TypeError : Cannot match infered signature forall a b . a -> b -> b with declared signature forall a b . a -> b -> a at fileName 3:1
