# UnboundVariableInType

When defining functions, you should declare all type variables in the forall clause :

```
sig const = forall a b . a -> b -> a;
let const a b = a;

sig wrong = forall b . a -> b -> a;
```
>>>TypeError : Type variable not in scope : a at fileName 5:1
