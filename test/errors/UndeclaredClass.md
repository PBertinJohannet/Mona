# UndeclaredClass

This error arise when you try to declare an instance for a class that does not exist.

```
inst List of Foo  = {
  let hey = 1;
}
```
>>>TypeError : Undeclared class : Foo at fileName 2:1
