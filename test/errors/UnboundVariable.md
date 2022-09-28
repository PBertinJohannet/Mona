# UnboundVariable

This error is raised when you reference a variable that was not declared.
```
let wrong a = a + b;
```
>>>TypeError : Variable not in scope : "b" at fileName 2:1
