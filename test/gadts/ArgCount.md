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

let correct f v = case v of
  (Both j k) -> f,
  (Just j) -> Just j;
```
>>>compiled successfully
*correct:forall a . Maybe a -> Maybe a -> Maybe a

## Same type than outside case, twice

infering correct type for the function arguments.

```
let correct f v = case v of
  (Both j k) -> f,
  (Just j) -> Just j,
  (Nothing) -> f;
```
>>>compiled successfully
*correct:forall a . Maybe a -> Maybe a -> Maybe a


## Same type than outside case, twice but in args

infering correct type for the function arguments.

```
let const a b = a;

let correct f v = case v of
  (Both j k) -> const f j,
  (Just j) -> const j f,
  (Nothing) -> f;
```
>>>compiled successfully
*correct:forall a . a -> Maybe a -> a


## Same type than outside case, twice but different

infering correct type for the function arguments.

```
let correct f g v = case v of
  (Both j k) -> f,
  (Just j) -> Just j,
  (Nothing) -> g;
```
>>>compiled successfully
*correct:forall a . Maybe a -> Maybe a -> Maybe a -> Maybe a



## Identity

Basic identity.

```
let id v = case v of
  (Both j k) -> Both j k,
  (Just j) -> Just j,
  (Nothing) -> Nothing;
```
>>>compiled successfully
*id:forall a . Maybe a -> Maybe a



## Unused variable.

the i variable of the last branch is not used.

```  
let id v = case v of
  (Nothing) -> Nothing,
  (Just i) -> Just i,
  (Both j i) -> Just j;
```
>>>compiled successfully
*id:forall a . Maybe a -> Maybe a


## First branch easy 

when starting with the easy branch, we also have a good result (the unification would prefer keeping the type of the last branch.)
```
let fmap f v = case v of
  (Just i) -> Just (f i),
  (Nothing) -> Nothing;
```
>>>compiled successfully
*fmap:forall a b . (a -> b) -> Maybe a -> Maybe b



## Only one branch

When there is only one branch, we still infer a correct type.
```
let fmap f v = case v of
  (Just i) -> Just (f i);
```
>>>compiled successfully
*fmap:forall a b . (a -> b) -> Maybe a -> Maybe b

## Both variables unused

When both variables are unused and the first variable was modified we still infer correctly (was pretty tricky this one).
```
let id f v = case v of
  (Just i) -> Just (f i),
  (Both i j) -> Nothing;
```
>>>compiled successfully
*id:forall a b . (a -> b) -> Maybe a -> Maybe b

## Nested Maybes with hiden unused variable.

The fact that the type variable of the nothing is unused could be hidden by the call to const.

```
let const a b = a;

let id f v = case v of
  (Just i) -> Just (f i),
  (Just i) -> const Nothing (f i);
```
>>>compiled successfully
*id:forall a b . (a -> b) -> Maybe a -> Maybe b
*const:forall a b . a -> b -> a

## Generalize correctly

This one should keep the second argument. 

```
let x = (snd (P (Just 1) (2) (Just 1))) + 2;
```
>>>compiled successfully
*x:Int

## But not too early

And the first should be different.
```
let x = (snd (P (1) (Just 2) (Just 1))) + 2;
```
>>>TypeError : Cannot unify : Int with Maybe Int at fileName 19:1 at fileName 19:41


## Unifying by function

Here the first type must unify with the result of f applied to the second.
```
let secondToFirst f x = case x of
  (P a b c) -> f b,
  (P a b c) -> a;
```
>>>compiled successfully
*secondToFirst:forall a b c . (b -> a) -> Prod a b c -> a

## Unifying by local function

Same than preceding but the function is created inside.
```
let secondToFirst x = (\f -> case x of 
  (P a b c) -> f Nothing b,
  (P d e f) -> d) (\a b -> b);
```
>>>compiled successfully
*secondToFirst:forall a b . Prod a a b -> a

## Catch all
Dont forget this one.
```
let catchAll x = case x of
  (Just i) -> Both i i,
  b -> b;

```
>>>TypeError : Cannot unify roots of candidate type : Maybe and 'g at fileName 19:1

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
