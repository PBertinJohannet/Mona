# ArgCount

When the constructors have different argument counts it should also work.
This is especially tricky when constructors have zero arguments or when variables are unused.
Here is a list of examples using these types : 
```
  
data Prod a b c = 
  | P = a -> b -> c -> Prod a b c;

data Maybe a =
  | Both = a -> a -> Maybe a;
  | Just = a -> Maybe a;
  | Nothing = Maybe a;
  
let snd a = case a of  
  (P a b c) -> b,
  (P a b c) -> b,
  (P a b c) -> b;

let main = printInt 4;
```

## Same type than outside case 

infering correct type for the function arguments.

```
sig correct = forall a . Maybe a -> Maybe a -> Maybe a;
let correct f v = case v of
  (Both j k) -> f,
  (Just j) -> Just j;
```
>>>compiled successfully


## Same type than outside case, twice

infering correct type for the function arguments.

```
sig correct = forall a . Maybe a -> Maybe a -> Maybe a;
let correct f v = case v of
  (Both j k) -> f,
  (Just j) -> Just j,
  (Nothing) -> f;
```
>>>compiled successfully

## Same type than outside case, twice but in args

infering correct type for the function arguments.

```
let const a b = a;

sig correct = forall a . Maybe a -> Maybe a -> a;
let correct f v = case v of
  (Both j k) -> const f j,
  (Just j) -> const j f,
  (Nothing) -> f;
```
>>>compiled successfully


## Same type than outside case, twice but different

infering correct type for the function arguments.

```
sig correct = forall a . Maybe a -> Maybe a -> Maybe a -> Maybe a;
let correct f g v = case v of
  (Both j k) -> f,
  (Just j) -> Just j,
  (Nothing) -> g;
```
>>>compiled successfully


## Identity

Basic identity.

```
sig id = forall a . Maybe a -> Maybe a;
let id v = case v of
  (Both j k) -> Both j k,
  (Just j) -> Just j,
  (Nothing) -> Nothing;
```
>>>compiled successfully


## Unused variable.

the i variable of the last branch is not used.

```  
sig id = forall a . Maybe a -> Maybe a;
let id v = case v of
  (Nothing) -> Nothing,
  (Just i) -> Just i,
  (Both j i) -> Just j;
```
>>>compiled successfully

## First branch easy 

when starting with the easy branch, we also have a good result (the unification would prefer keeping the type of the last branch.)
```
sig correct = forall a b . (a -> b) -> Maybe a -> Maybe b;
let fmap f v = case v of
  (Just i) -> Just (f i),
  (Nothing) -> Nothing;
```
>>>compiled successfully


## Only one branch

When there is only one branch, we still infer a correct type.
```
sig correct = forall a b . (a -> b) -> Maybe a -> Maybe b;
let id f v = case v of
  (Just i) -> Just (f i);
```
>>>compiled successfully

## Both variables unused

When both variables are unused and the first variable was modified we still infer correctly (was pretty tricky this one).
```
sig correct = forall a b . (a -> b) -> Maybe a -> Maybe b;
let id f v = case v of
  (Just i) -> Just (f i),
  (Both i j) -> Nothing;
```
>>>compiled successfully

## Nested Maybes with hiden unused variable.

The fact that the type variable of the nothing is unused could be hidden by the call to const.

```
let const a b = a;

sig correct = forall a b . (a -> b) -> Maybe a -> Maybe b;
let id f v = case v of
  (Just i) -> Just (f i),
  (Just i) -> const Nothing (f i);
```
>>>compiled successfully


## Generalize correctly

This one should keep the second argument. 

```
sig correct = Int;
let x = (snd (P (Just 1) (2) (Just 1))) + 2;
```
>>>compiled successfully

## But not too early

And the first should be different.
```
let x = (snd (P (1) (Just 2) (Just 1))) + 2;
```
>>>TypeError : Cannot unify : Int with Maybe Int at fileName 19:1 at fileName 19:41


## Unifying by function

Here the first type must unify with the result of f applied to the second.
```
sig secondToFirst = forall a b c d . (b -> b) -> Prod a b c -> a;  
let secondToFirst f x = case x of
  (P a b c) -> f b,
  (P d e f) -> d;
```
>>>compiled successfully

## Unifying by local function

Same than preceding but the function is created inside.
```
let secondToFirst x = (\f -> case x of 
  (P a b c) -> f Nothing b,
  (P d e f) -> d) (\a b -> b);
```
>>>compiled successfully

## Catch all
Dont forget this one.
```
let catchALl x = case x of
  (Just i) -> Both i i,
  b -> b;

```
>>>compiled successfully

## No dependency between multiple branches.

Combination of many of the above.

```
let secondToFirst x = 
  (\f -> case x of 
    (P (Just a) b c) -> f (Both a a),
    (P a b c) -> case a of 
      (Just i) -> f (Nothing),
      b -> f b)
  (\v -> case v of
      (Nothing) -> Nothing,
      (Just i) -> Just i,
      (Both j i) -> Just j);
```
>>>TypeError : Dependency between 'ab and 'ad at fileName 19:1
