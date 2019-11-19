write a file with multiple tests for every line here :
src/Dispatch.hs
78:  (throwError ||| return) a
89:      Nothing -> throwError MainNotFound

src/Sig.hs
41:        Nothing -> throwErrorV $ UnboundVariable name

src/Run.hs
107:      (throwError $ ShouldNotHappen $ "cannot find variable " ++ x)
113:        throwError $ ShouldNotHappen $ "applying a non function " ++ pretty a ++ " to an arg"
116:    e -> throwError $ ShouldNotHappen $ "applying fix to a non function " ++ pretty t1
124:      a -> throwError $ ShouldNotHappen $ "applying a non function pattern " ++ pretty a
130:  (a, b) -> throwError $ ShouldNotHappen $ "Non func in native " ++ pretty a))
142:  e -> throwError $ ShouldNotHappen $ "applying a non function " ++ pretty a ++ " to an arg " ++ pretty e ++ "\n"
152:  throwError $ ShouldNotHappen $ "Pattern called on non Object" ++ val ++ " at : " ++ s
164:  e -> throwError $ ShouldNotHappen $ "excpecting an int to PrintInt" ++ pretty e)
174:  _ -> throwError $ ShouldNotHappen "excpecting a char to PrintChar")
179:  (a, b) -> throwError $ ShouldNotHappen $ "Non integer in equals " ++ pretty a))
185:  (a, b) -> throwError $ ShouldNotHappen $ "Non integer in native " ++ pretty a))

src/DataTypes.hs
55:inferKinds name tvars [] = throwError $ NoConstructor name
108:    Nothing -> throwError $ DoesNotAppear name
157:unifies k1 k2 = throwError $ KindUnificationFail k1 k2

src/Typeclass.hs
45:    Nothing -> throwError $ TypeError (UndeclaredClass s) [loc]
57:group [] (i:is) = throwError $ UndeclaredClass $ sel1 (Prelude.head i)
58:group (c:cs) _ = throwError $ MultipleDecl $ sel1 (Prelude.head c)
102:      else throwError $ TypeError (UnificationFail (tvar a) (tvar a')) (l:locs)

src/Infer.hs
59:throwErrorV :: MonadError TypeError m => TypeErrorV -> m a
60:throwErrorV variant = throwError (TypeError variant [])
65:    withLoc (TypeError variant a) = throwError $ TypeError variant (loc:a)
127:  Left err -> throwError err
141:  Left err -> throwError err
219:    Nothing -> throwErrorV $ UnboundVariable $ show x
281:                  else throwErrorV $ SignatureMismatch sub
298:checkStrict t1 t2 _ = throwErrorV $ UnificationFail t1 t2
310:unifies t1 t2 = throwErrorV $ UnificationFail t1 t2
318:unifyMany t1 t2 = throwErrorV $ UnificationMismatch t1 t2
354:    Nothing -> throwErrorV $ UnknownClass n
365:satisfyInsts (IsIn c t) [] = throwErrorV $ NotInClass c t
371:  s <- unifies t' t `catchError` const (throwErrorV $ NotInClass c t)
377:         | occursCheck a t = throwErrorV $ InfiniteType t



# Then

make it possible to run the code.

create a few examples.
