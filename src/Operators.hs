{-# LANGUAGE LambdaCase #-}

module Operators (
  toFunc,
  Binop(..),
  allOps
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
allOps  = ("if", ifOp):ops

-- if :: Bool -> a -> a
ifOp :: Scheme
ifOp = Forall [TV "a"]
  $ typeBool `TArr` (TVar (TV "a") `TArr` (TVar (TV "a") `TArr` (TVar (TV "a"))))

-- (bool -> (a -> (a -> a)))

ops :: [(Name, Scheme)]
ops = fmap (\(name, (a, b, c)) -> (name, Forall [] $ a `TArr` (b `TArr` c)))
  [("+", (typeInt, typeInt, typeInt)),
  ("*", (typeInt, typeInt, typeInt)),
  ("-", (typeInt, typeInt, typeInt)),
  ("==", (typeInt, typeInt, typeBool))]
