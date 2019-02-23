{-# LANGUAGE LambdaCase #-}

module Operators (
  toFunc,
  Binop(..),
  allOps,
  allClasses,
  allKinds
  ) where

import Type

type Name = String;
data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

toFunc :: Binop -> String
toFunc = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Eql -> "="


allOps :: [(Name, Scheme)]
allOps  = [
  ("show", showOp),
  ("[]", emptyList),
  ("++", listConcat),
  (":", listOp),
  ("if", ifOp)] ++ ops

showOp :: Scheme
showOp = Forall [var "a"] $ Qual [IsIn "Show" a] $ a `mkArr` mkList typeChar
  where a = tvar "a"

emptyList :: Scheme
emptyList = Forall [var "a"] $ Qual []  $ mkList $ tvar "a"

listConcat :: Scheme
listConcat = Forall [var "a"] $ Qual []  $ la `mkArr` (la `mkArr` la)
  where la = mkList $ tvar "a"

listOp :: Scheme
listOp = Forall [var "a"] $ Qual [] $ tvar "a" `mkArr` (la `mkArr` la)
  where la = mkList $ tvar "a"

-- if :: Bool -> a -> a
ifOp :: Scheme
ifOp = Forall [var "a"]
  $ Qual []
  $ typeBool `mkArr` (tvar "a" `mkArr` (tvar "a" `mkArr` tvar "a"))


allClasses :: [(String, Class)]
allClasses = [
  ("Ord", (["Eq"],
       [
       Qual [] $ IsIn "Ord" typeBool,
       Qual [] $ IsIn "Ord" typeInt,
       Qual [IsIn "Ord" $ tvar "a"] $ IsIn "Ord" $ mkList $ tvar "a"
       ])),
  ("Show", ([],
      [
      Qual [] $ IsIn "Show" typeBool,
      Qual [] $ IsIn "Show" typeInt,
      Qual [IsIn "Show" $ tvar "a"] $ IsIn "Show" $ mkList $ tvar "a"
      ]))]

allKinds :: [(String, Scheme)]
allKinds = [
    ("List", Forall [var "a"] $ Qual [] $ tvar "a" `mkArr` tvar "a"),
    ("Int", Forall [] $ Qual [] $ tvar "a"),
    ("|", Forall [var "a"] $ Qual [] $ tvar "a" `mkArr` (tvar "a" `mkArr` tvar "a")),
    ("Bool", Forall [] $ Qual [] $ tvar "a"),
    ("Char", Forall [] $ Qual [] $ tvar "a")
    ]

-- (bool -> (a -> (a -> a)))

ops :: [(Name, Scheme)]
ops = fmap (\(name, (a, b, c)) -> (name, Forall [] $ Qual []  $ a `mkArr` (b `mkArr` c)))
  [("+", (typeInt, typeInt, typeInt)),
  ("*", (typeInt, typeInt, typeInt)),
  ("-", (typeInt, typeInt, typeInt)),
  (".", (tvar "b" `mkArr` tvar "c",
        tvar "a" `mkArr` tvar "b",
        tvar "a" `mkArr` tvar "c")),
  ("==", (typeInt, typeInt, typeBool))]
