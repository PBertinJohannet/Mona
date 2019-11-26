# InfiniteKind
This error arise when the typechecker encounters a possibly infinite kind.
```
data Fixed f = | Fix = f f -> Fixed f;
```
>>>DataDeclError : (InfiniteKind) Could not construct the kind f -> 'd at fileName 2:1
