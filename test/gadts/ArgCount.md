# ArgCount

When the constructors have different argument counts it should also work.
This is especially tricky when constructors have zero arguments or when variables are unused.
Here is a list of examples using this type : 
```

data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let main = printInt 4;
```

## Same type than outside case 

infering correct type for the function arguments.

```
let correct f v = case v of
  (Both j k) -> f,
  (Just j) -> Just j,
  (Nothing) -> f;
```
>>>compiled successfully


## Identity

Basic identity.

```
let id f v = case v of
  (Both j k) -> Both j k,
  (Just j) -> Just j,
  (Nothing) -> Nothing;
```
>>>compiled successfully


## Unused variable.

the i variable of the last branch is not used.

```  
let id v = case v of
  (Nothing) -> Nothing,
  (Just i) -> Just i,
  (Both j i) -> Just j;
```
>>>compiled successfully

## First branch easy 

when starting with the easy branch, we also have a good result (the unification would prefer keeping the type of the last branch.)
```
let fmap f v = case v of
  (Just i) -> Just (f i),
  (Nothing) -> Nothing;
```
>>>compiled successfully


## Only one branch

When there is only one branch, we still infer a correct type.
```
let id f v = case v of
  (Just i) -> Just (f i);
```
>>>compiled successfully

## Both variables unused

When both variables are unused and the first variable was modified we still infer correctly (pretty tricky this one).
```
let id f v = case v of
  (Just i) -> Just (f i),
  (Both i j) -> Nothing;
```
>>>compiled successfully

## Nested Maybes with hiden unused variable.

The fact that the type variable of the nothing is unused could be hidden by the call to const.

```
let const a b = a;

let id f v = case v of
  (Just i) -> Just (f i),
  (Just i) -> const Nothing (f i);
```
>>>compiled successfully

## Do not generalize too easly

This one should keep the second argument. 

```
a b c -> b
a b c -> b
a b c -> b
```
>>>compiled successfully

## oh oh oh
completer quand je le comprend. 
```
a b c -> f b (f :: b -> m)
d e f -> d
```
>>>compiled successfully

## brutal
pareil que le précédent.
```
let f = const(:: a -> b -> a) m
a b c -> f b (f :: b -> m)
d e f -> d
```
>>>compiled successfully

## aussi
le faire dans un autre fichier celui la ptet.
```
voir si on peut faire apparaitre un type dans d'autres réconciliations et quand meme vouloir qu'il soit refinable.
```
>>>compiled successfully
