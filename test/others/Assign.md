# Assign

A bug that I found while checking the kind system that also exists at the type system.
```

data Star = | Star = Star;

sig assign = forall b . (b -> Star) -> b -> Star;
let assign f a = f a;

sig maybe = Star -> Star;
let maybe a = a;

sig andMore = ((Star -> Star) -> Star) -> Star
let andMore f = f maybe;

let fromStar = \(Star) -> 1;

```

## Should work

```
let andmoremaybe = assign maybe Star;
let main = printInt (fromStar andmoremaybe);
```
>>>compiled successfully

## Should not work
```
let andmoremaybe = assign andMore Star;
let main = printInt (fromStar andmoremaybe);
```
>>>TypeError : Cannot unify : (Star -> Star) -> Star with Star at fileName 18:1 at fileName 18:35
