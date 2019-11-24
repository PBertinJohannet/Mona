# KindUnificationFail
This error happens if there is a kind mimatch in a constructor.
When you declare a constructor the kind of the data is infered.

Check `others/PolyKinds.md` to see more advanced examples.

Here for example, the infered type is `* -> * -> *`
The error can happen in two cases :

```
data MyTuple a b =
  | MyTuple = a -> b -> MyTuple a b;
```

## Mismatch in a constructor signature

Here, `MyTuple` is of kind `* -> * -> *` and it expects a type of kind `*` as first arguments.

```
data Wrong a =
  | Wrong = MyTuple MyTuple a -> Wrong a;
```
>>>DataDeclError : (KindUnificationFail) Could not unify kinds * and  * -> * -> *

## Mismatch between multiple constructors

If the infered kinds are different between the constructors you also have a mismatch.
Here the `a` type parameter is declared as `* -> * -> *` in the first constructor and `*` in the second.

```
data AlsoWrong a b =
  | Also = AlsoWrong MyTuple Int;
  | Wrong = AlsoWrong Int MyTuple;
```
>>>DataDeclError : (KindUnificationFail) Could not unify kinds * -> * -> * and  *
