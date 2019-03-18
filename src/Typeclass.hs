{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Typeclass where

import Type
import Control.Monad.Except
import Control.Monad.Writer
import Infer
import Data.List (find, sortOn, groupBy)
import Sig
import Syntax
import Control.Arrow
import qualified Env (lookup)
import Env (Envs, addClass, classEnv, addInstance)
import Prelude hiding (head)
import qualified Prelude
import qualified Data.Map as Map
import Pretty

instance Pretty a => Pretty (String, a) where
  pretty (k, c) = k ++ " = " ++ pretty c ++ "\n"

instance Pretty (ClassDecl, [InstDecl]) where
  pretty (c, i) = pretty c ++ " \n => \n "++ pretty i ++ "\n"

type AddClass a = ExceptT TypeError (Writer String) a
-- (string, a) (string, b)
mergeInstances :: [ClassDecl] -> [InstDecl] -> AddClass [(ClassDecl, [InstDecl])]
mergeInstances cls ins =
  let clsMap = Map.fromList ((sel1 &&& id) <$> cls) in
  (fmap snd . Map.toList) <$> mergeMaps ((sel1 &&& id) <$> ins) clsMap

sel1 (x, _, _) = x
eq1 (x, _, _) (y, _, _) = x == y

mergeMaps :: [(String, InstDecl)]
 -> Map.Map String ClassDecl
 -> AddClass (Map.Map String (ClassDecl, [InstDecl]))
mergeMaps [] cls = return $ (\e -> (e, [])) <$> cls
mergeMaps ((s, d):ds) cls = do
  inner <- mergeMaps ds cls
  case Map.lookup s inner of
    Just (c, insts) -> return $ Map.alter (fmap $ second (d:)) s inner
    Nothing -> throwError $ UndeclaredClass s


group :: [[ClassDecl]] -> [[InstDecl]] -> AddClass [(ClassDecl, [InstDecl])]
group ([c]:cs) (i:is) =
  if sel1 c == sel1 (Prelude.head i)
    then do
      base <- group cs is
      return $ (c, i):base
    else do
      base <- group cs (i:is)
      return $ (c, i):base
group [] (i:is) = throwError $ UndeclaredClass $ sel1 (Prelude.head i)
group (c:cs) _ = throwError $ MultipleDecl $ sel1 (Prelude.head c)


addInstances :: [InstDecl] -> Envs -> AddClass Envs
addInstances [] env = return env
addInstances ((name, tp, _):is) env = do
  env <- addInstances is env
  return env{classEnv = addInstance (classEnv env) name $ Qual [] $ IsIn [] tp}


addClasses :: [ClassDecl] -> Envs -> AddClass Envs
addClasses [] env = return env
addClasses ((name, v, sigs):ss) env = do
  env1 <- addClasses ss env
  let env2 = env1{classEnv = addClass (classEnv env1) (name, ([], []))}
  addSigs (second (withPred v (IsIn name $ tvar v)) <$> sigs) env2

runAddClasses :: [ClassDecl] -> [InstDecl] -> Envs -> AddClass (Envs, [(Scheme, Expr)])
runAddClasses c i env = do
  cls <- mergeInstances c i
  tell $ "add classes : " ++ pretty cls ++ "\n"
  env <- addClasses c env
  env <- addInstances i env
  schems <- mconcat <$> mapM checkSigs cls
  return (env, schems)

checkSigs :: (ClassDecl, [InstDecl]) -> AddClass [(Scheme, Expr)]
checkSigs (c, i) = mconcat <$> mapM (checkSig c) i

checkSig :: ClassDecl -> InstDecl -> AddClass [(Scheme, Expr)]
checkSig (_, tv, funcs) (_, t, exprs) = do
  let baseSubst = Map.singleton tv t
  tell $ "checking : "++ pretty (snd <$> funcs) ++ " vs : " ++ pretty exprs ++ "\n"
  groupStrict funcs exprs

groupStrict :: [(String, a)] -> [(String, b)] -> AddClass [(a, b)]
groupStrict a b = foldM inGroup [] (zip (sortOn fst a) (sortOn fst b))
  where
    inGroup lst ((a, b), (a', b')) =
      if a == a'
      then return $ (b, b'):lst
      else throwError $ UnificationFail (tvar a) (tvar a')
