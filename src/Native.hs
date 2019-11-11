{-# LANGUAGE LambdaCase #-}

module Native (
  toFunc,
  Binop(..),
  allOps,
  allClasses,
  allKinds,
  allOpsTypes,
  allNatives,
  ) where

import Type
import Run
import qualified Data.Map as Map

type NativeFunc = (Name, Scheme, Value)

type Name = String;
data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

toFunc :: Binop -> String
toFunc = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Eql -> "="

allNatives :: Map.Map String Value
allNatives = Map.fromList ((\(n, _, r) -> (n, r)) <$> allOps)

allOpsTypes :: [(Name, Scheme)]
allOpsTypes = (\(a, b, c) -> (a, b)) <$> allOps

allOps :: [NativeFunc]
allOps  = endl:printInt:printChar: ops

printInt :: NativeFunc
printInt = ("printInt", printIntOp, runPrintInt)

printChar :: NativeFunc
printChar = ("printChar", printCharOp, runPrintChar)

endl :: NativeFunc
endl = ("endl", Forall [] $ Qual [] typeChar, runEndl)

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

printIntOp :: Scheme
printIntOp = Forall []
  $ Qual []
  $ typeInt `mkArr` TApp typeIO typeUnit

printCharOp :: Scheme
printCharOp = Forall []
  $ Qual []
  $ typeChar `mkArr` TApp typeIO typeUnit

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

{-allKinds :: [(String, Scheme)]
allKinds = [
    ("List", Forall [var "a"] $ Qual [] $ tvar "a" `mkArr` tvar "a"),
    ("Int", Forall [] $ Qual [] $ tvar "a"),
    ("(->)", Forall [var "a"] $ Qual [] $ tvar "a" `mkArr` (tvar "a" `mkArr` tvar "a")),
    ("Bool", Forall [] $ Qual [] $ tvar "a"),
    ("Char", Forall [] $ Qual [] $ tvar "a"),
    ("IO", Forall [var "a"] $ Qual [] $ tvar "a" `mkArr` tvar "a")
    ]-}
allKinds :: [(String, Kind)]
allKinds = [
    ("Int", Star),
    ("(->)", Kfun Star (Kfun Star Star)),
    ("Bool", Star),
    ("Char", Star),
    ("IO", Kfun Star Star)
  ]

-- (bool -> (a -> (a -> a)))

ops :: [NativeFunc]
ops = fmap (\(name, (a, b, c), r) -> (name, Forall [] $ Qual []  $ a `mkArr` (b `mkArr` c), r))
  [("+", (typeInt, typeInt, typeInt), mkRun "+" (+)),
  ("*", (typeInt, typeInt, typeInt), mkRun "*"(*)),
  ("-", (typeInt, typeInt, typeInt), mkRun "-"(-)),
  (".", (tvar "b" `mkArr` tvar "c",
        tvar "a" `mkArr` tvar "b",
        tvar "a" `mkArr` tvar "c"), runCompose),
  ("==", (typeInt, typeInt, typeBool), runEquals)]
