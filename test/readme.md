# tests

You can write tests as md files, just include some base code :

```
let add a = \b -> a + b;
```

## Subtests

By using subtests you can add specific code to be tested.

```
let main = printInt (add 1 2);
```
>>>compiled successfully

## Subtest 2
```
let main = add 1 2 3;
```
>>>TypeError : Cannot unify : Int with Int -> 'c at fileName 5:1 at fileName 5:20
