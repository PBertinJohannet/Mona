{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Typeclass where

import Type
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Infer
import Data.List (find, sortOn, groupBy)
import Sig
import Syntax
import Control.Arrow
import qualified Env (lookup)
import Env (Envs(..), addClass, classEnv, addInstance, Env, dataEnv)
import Prelude hiding (head)
import qualified Prelude
import qualified Data.Map as Map
import Pretty
import Subst

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
  let Envs e _ _ _ = env
  tp <- replaceConsTypes [] e tp
  return env{classEnv = addInstance (classEnv env) name $ Qual [] $ IsIn [] tp}

addClasses :: [ClassDecl] -> Envs -> AddClass Envs
addClasses [] env = return env
addClasses ((name, v, sigs):ss) env = do
  env1 <- addClasses ss env
  let env2 = env1{classEnv = addClass (classEnv env1) (name, ([], []))}
  addSigs (second (withPred v (IsIn name $ tvar v)) <$> sigs) env2

runAddClasses :: [ClassDecl] -> [InstDecl] -> Envs -> AddClass (Envs, [InstCheck])
runAddClasses c i env = do
  cls <- mergeInstances c i
  tell $ "add classes : " ++ prettyL cls ++ "\n"
  env <- addClasses c env
  env <- addInstances i env
  schems <- mconcat <$> mapM (checkSigs $ dataEnv env) cls
  return (env, schems)

checkSigs :: Env -> (ClassDecl, [InstDecl]) -> AddClass [InstCheck]
checkSigs env (c, i) = mconcat <$> mapM (checkSig env c) i

checkSig :: Env -> ClassDecl -> InstDecl -> AddClass [InstCheck]
checkSig env (_, tv, funcs) (_, t, exprs) = do
  t <- replaceConsTypes [] env t
  let baseSubst = Map.singleton (var tv) t
  groupStrict env (second (apply baseSubst) <$> funcs) (fmap (pretty t,) exprs)

groupStrict :: Env -> [(String, Scheme)] -> [(String, (String, Expr))] -> AddClass [InstCheck]
groupStrict dEnv a b = foldM inGroup [] (zip (sortOn fst a) (sortOn (fst . snd) b))
  where
    inGroup lst ((a, b), (s, (a', b'))) =
      if a == a'
      then do
        b <- replaceConsTypes [] dEnv b
        return $ (unwords [a, s], b, b'):lst
      else throwError $ UnificationFail (tvar a) (tvar a')
