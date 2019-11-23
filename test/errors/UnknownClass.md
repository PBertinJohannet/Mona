# UnknownClass
This error arises when defining a function and putting a constraint on a variable.
If the class referenced by the constraint cannot be found, the error will be thrown.
here the class `Hello` is not known.
```
sig wrong = forall a . Hello a => a -> Int;
let wrong a = 2;
let main = printInt (wrong 2);
```
>>>TypeError : Unknown class : Hello at fileName 4:1 at fileName 4:22
