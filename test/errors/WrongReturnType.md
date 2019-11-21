# DoesNotAppear

This errors arise when you declare a signature for a constructor of a type that does not ends with the type.
For example here the `That` constructor does not return a `That`. It is not a constructor of `That` then.

```
data That = | Thats = List Int;
```
>>>DataDeclError : (WrongReturnType) signature's return type should be an application of That but it is an application of List (in : List Int)
