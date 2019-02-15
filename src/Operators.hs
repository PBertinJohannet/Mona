{-# LANGUAGE LambdaCase #-}

module Operators (
  toFunc,
  Binop(..),
  allOps,
  allClasses
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
  ("[]", emptyList),
  ("++", listConcat),
  (":", listOp),
  ("if", ifOp)] ++ ops

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
       ]))]

-- (bool -> (a -> (a -> a)))

ops :: [(Name, Scheme)]
ops = fmap (\(name, (a, b, c)) -> (name, Forall [] $ Qual []  $ a `mkArr` (b `mkArr` c)))
  [("+", (typeInt, typeInt, typeInt)),
  ("*", (typeInt, typeInt, typeInt)),
  ("-", (typeInt, typeInt, typeInt)),
  ("==", (typeInt, typeInt, typeBool))]
