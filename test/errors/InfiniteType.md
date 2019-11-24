# InfiniteType
This error arise when the typechecker encounters a possibly infinite type. For example, it is impossible to create `fix` without a wrapper.
```
let fixed f = f f;
```
>>>TypeError : Cannot create infinite type : 'a -> 'b at fileName 2:1 at fileName 4:1
