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
import Env (Envs(..), KEnv, addClass, classEnv, addInstance, Env, dataEnv)
import Prelude
import qualified Prelude
import qualified Data.Map as Map
import Pretty
import Subst

type AddClass a = ExceptT TypeError (Writer String) a
-- (string, a) (string, b)
mergeInstances :: [ClassDecl] -> [InstDecl] -> AddClass [(ClassDecl, [InstDecl])]
mergeInstances cls ins =
  let clsMap = Map.fromList ((sel2 &&& id) <$> cls) in
  (fmap snd . Map.toList) <$> mergeMaps ((sel12 &&& id) <$> ins) clsMap

third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f (a, b, c) = (a, b, f c)
sel2 (_, x, _, _) = x
sel2of3 (_, x, _) = x
sel12 (loc, x, _, _) = (loc, x)
eq21 (_, x, _, _) (y, _, _) = x == y

mergeMaps :: [((Location, String), InstDecl)]
 -> Map.Map String ClassDecl
 -> AddClass (Map.Map String (ClassDecl, [InstDecl]))
mergeMaps [] cls = return $ (\e -> (e, [])) <$> cls
mergeMaps (((loc, s), d):ds) cls = do
  inner <- mergeMaps ds cls
  case Map.lookup s inner of
    Just (c, insts) -> return $ Map.alter (fmap $ second (d:)) s inner
    Nothing -> throwError $ TypeError (UndeclaredClass s) [loc]

addInstances :: [InstDecl] -> Envs -> AddClass Envs
addInstances [] env = return env
addInstances ((loc, name, tp, _):is) env = do
  env <- addInstances is env
  let Envs e _ _ _ = env
  tp <- replaceConsTypes [] e tp
  return env{classEnv = addInstance (classEnv env) name $ Qual [] $ IsIn [] tp}

addClasses :: [ClassDecl] -> Envs -> AddClass Envs
addClasses [] env = return env
addClasses ((loc, name, v, sigs):ss) env = do
  env1 <- addClasses ss env
  let env2 = env1{classEnv = addClass (classEnv env1) (name, ([], []))}
  addSigs (third (withPred v (IsIn name $ tvar v)) <$> sigs) env2

runAddClasses :: [ClassDecl] -> [InstDecl] -> Envs -> AddClass (Envs, [InstCheck])
runAddClasses c i env = do
  cls <- mergeInstances c i
  tell $ "add classes : " ++ prettyL cls ++ "\n"
  env <- addClasses c env
  env <- addInstances i env
  schems <- mconcat <$> mapM (checkSigs $ dataEnv env) cls
  return (env, schems)

checkSigs :: KEnv -> (ClassDecl, [InstDecl]) -> AddClass [InstCheck]
checkSigs env (c, i) = mconcat <$> mapM (checkSig env c) i

checkSig :: KEnv -> ClassDecl -> InstDecl -> AddClass [InstCheck]
checkSig env (locC, _, tv, funcs) (locI, _, t, exprs) = do
  t <- replaceConsTypes [] env t
  let baseSubst = Map.singleton (var tv) t
  groupStrict env [locC, locI] (third (apply baseSubst) <$> funcs) (fmap (pretty t,) exprs)

groupStrict :: KEnv -> [Location] -> [(Location, String, Scheme)] -> [(String, (String, Expr))] -> AddClass [InstCheck]
groupStrict dEnv locs a b = foldM inGroup [] (zip (sortOn sel2of3 a) (sortOn (fst . snd) b))
  where
    inGroup lst ((l, a, b), (s, (a', b'))) =
      if a == a'
      then do
        b <- replaceConsTypes [] dEnv b `withErrorLoc` l
        return $ (locs, unwords [a, s], b, b'):lst
      else throwError $ TypeError (NotAClassFunction a') (l:locs)
