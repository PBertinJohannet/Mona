# Depths

With different constraints at different depths. (todo check infered signature)
```
data C a =
  | CI = Int -> C Int; 
  | CA = a -> C a;
  | CC = C (C a) -> C (C a);
  | CR = C (f a) -> C (f a);
  | CCI = C (C Int) -> C (C Int);

```

## Correct CA

Here we just need to check the final signature.
```

let correctCA c = case c of
  (CA (CI i)) -> i,
  (CA (CA i)) -> i;

let AlsoCorrectCA c = case c of
  (CA (CI i)) -> i,
  (CA (CA i)) -> i + 1;

```

## Incorrect CA

todo


## Impose in depth

Int est imposé car pas prévu par le constructeur CC.
```
let imposeInt c = case c of
  (CC (CI i)) -> 1,
  (CC (CA i)) -> i + 1;

let imposeIntAlso c = case c of
  (CC (CA a)) -> 1,
  (CC (CI i)) -> 1;
```