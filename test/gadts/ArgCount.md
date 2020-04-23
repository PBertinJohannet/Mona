# ArgCount

When the constructors have different argument counts it should also work.

## Should work

this one is kinda tricky.

data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let fmap f v = case v of
  (Both j k) -> f,
  (Just j) -> Just j,
  (Nothing) -> f;

let main = printInt (toInt four);




# this one also



data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let fmap f v = case v of
  (Both j k) -> Both j k,
  (Just j) -> Just j,
  (Nothing) -> Nothing;

let main = printInt (toInt four);


# this one is broken af


# le cas simple aussi 

data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let id v = case v of
  (Nothing) -> Nothing,
  (Just i) -> Just i,
  (Both j i) -> Just j;

let main = printInt 4;

# broken aussi


data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let id f v = case v of
  (Just i) -> Just (f i),
  (Nothing) -> Nothing;

let main = printInt 4;

# niqué quand un seul cas. 


data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let id f v = case v of
  (Just i) -> Just (f i);

let main = printInt 4;


# celui la il faut qu'il marche aussi.

let id f v = case v of
  (Just i) -> Just (f i),
  (Both i j) -> Nothing;

# et enfin celui la :

data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let id f v = case v of
  (Just i) -> Just (f i),
  (Nothing) -> Nothing;

let main = printInt 4;


# aussi un autre

data Maybe a =
  | Both = a -> a -> Maybe a
  | Just = a -> Maybe a
  | Nothing = Maybe a;
  
let const a b = a;

let id f v = case v of
  (Just i) -> Just (f i),
  (Just i) -> const Nothing (f i);

let main = printInt 4;


# ne pas oublier celui la

a b c -> b
a b c -> b
a b c -> b


# oh oh oh 

a b c -> f b (f :: b -> m)
d e f -> d

# brutal 

let f = const(:: a -> b -> a) m
a b c -> f b (f :: b -> m)
d e f -> d
