# SignatureMismatch related tests

with these sigs :
```
sig const = forall a b . a -> b -> a;
sig constA = forall k . k -> k -> k;
let main = (const 1 2) + (constA 2 3);
```
## This should work

```
let const a b = a;
```
>>>compiled successfully

## This should work too

```
let constA a b = a;
```
>>>compiled successfully


## This should work too
```
let constA a b = b;
```
>>>compiled successfully

## This sould not

```
let const a b = b;
```
>>>TypeError : Cannot match infered signature forall a b . a -> b -> b with declared signature forall a b . a -> b -> a at fileName 7:1
