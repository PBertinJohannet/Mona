# NotAClassFunction
This error happens when you declare a function in an instance declaration that was not specified in the class declaration.

```
class IntClass i = {
  sig getInt = Int;
}

inst Int of IntClass = {
  let float = 2;
}
```
>>>TypeError : (NotAClassFunction) Function float was not declared in this class  at fileName 3:3 at fileName 2:1 at fileName 6:1
