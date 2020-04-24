# Type safe interpreter

This is going to be good.
Oh boy
```
data Prod a b = | P = a -> b -> Prod a b;
data Expr a =
  | Lift = a                       -> Expr a;
  | Tup  = Expr a -> Expr b        -> Expr (Prod a b);
  | Lam  = (Expr a -> Expr b)      -> Expr (a -> b);
  | App  = Expr (a -> b) -> Expr a -> Expr b;
  | Fix  = Expr (a -> a)           -> Expr a;
```

## This should be of the correct type 

```

let interpret f e = case e of
  (Lift a) -> a,
  (Tup a b) -> P (a) (b);
```
>>> compiled successfuly
