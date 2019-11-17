{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataTypes where
import Pretty
import Syntax
import Env (lookup, KEnv(..), Envs(..), letters, extends, extend)
import Type
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Control.Arrow
import Data.Maybe
import Data.List (intercalate, find)
import RecursionSchemes
import Native
import Infer (InferState(..), initInfer)
import Run (Value(..), Run, makeRunPat, makeRunCons)
import Subst (withFtv)

data DataDeclError
 = NoConstructor String
 | KindUnificationFail Kind Kind
 | DoesNotAppear String deriving (Show, Eq)

instance Pretty DataDeclError where
  pretty = show

runDataDecls :: [DataDecl] -> Envs -> ExceptT DataDeclError (Writer String) Envs
runDataDecls ds env = foldM runDataDecl env ds

runDataDecl :: Envs-> DataDecl -> ExceptT DataDeclError (Writer String) Envs
runDataDecl envs@(Envs d v cenv tast) (name, tvars, types) = do
  (kind, cons) <- runInferKind d $ inferKinds name tvars (snd <$> types)
  tell $ "for "  ++ name ++ " cons are : " ++ prettyL cons
  let consSchemes = makeCons (fst <$> types) name cons
  let patsSchemes = consToPat <$> consSchemes
  let v' = foldr (flip extend) v $ consSchemes ++ patsSchemes
  --tell $ "for : " ++ name ++ "s found : " ++  pretty res ++ "\n"
  return $ Envs (extend d (name, kind)) v' cenv tast

makeCons :: [String] -> String -> [Type] -> [(String, Scheme)]
makeCons consNames tpName types = do
  (n, t) <- zip consNames types
  let a = makeTypeConstant tpName t
  return (n, withFtv a)

inferKinds :: String -> [String] -> [Type] -> InferKind (Kind, [Type])
inferKinds name tvars [] = throwError $ NoConstructor name
inferKinds name tvars tps = do
  res <- makeBaseEnv name tvars
  kinds@((k, t):ks) <- mapM (inferConstraints res name) tps
  --tell $ "found all : [" ++ prettyL (k:ks) ++ "]"
  let res = mconcat (union k . fst <$> ks)
  sub <- unionSolve (Map.empty, res)
  let typesWithKinds =  mapKind (apply sub) . snd <$> kinds
  return (apply sub k, typesWithKinds)

type InferKind = ReaderT KEnv (StateT InferState (ExceptT DataDeclError (Writer String)))

runInferKind :: KEnv -> InferKind a -> ExceptT DataDeclError (Writer String) a
runInferKind kenv inf = evalStateT (runReaderT inf kenv) initInfer

lookupEnv :: String -> InferKind (Kind, Type)
lookupEnv s = do
  res <- Env.lookup s <$> ask
  case res of
    Just a -> return (a, TCon s a)
    Nothing -> return (KVar s, TVar $ TV s (KVar s))

fresh :: InferKind Kind
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ KVar (letters !! count s)

makeBaseEnv :: String -> [String] -> InferKind Constraints
makeBaseEnv name [] = return [(KVar name, Star)]
makeBaseEnv name (tvar:tvars) = do
  (KVar a) <- fresh
  next <- makeBaseEnv a tvars
  --tell $ "adding : " ++ pretty (name, Kfun (KVar tvar) (KVar a))
  return ((KVar name, Kfun (KVar tvar) (KVar a)):next)

type Constraints = [(Kind, Kind)]

instance Pretty (Kind, Kind) where
  pretty (a, b) = pretty a ++ " <=> " ++ pretty b

inferConstraints :: Constraints -> String -> Type -> InferKind (Kind, Type)
inferConstraints cs name t = do
  env <- ask
  --tell "==============="
  --tell $ "\n base : \n" ++ pretty env
  (cons, _, tp) <- generateConstraints t
  --tell $ pretty t
  --tell $ "cons found : " ++ show (fmap pretty (cs ++ cons)) ++ "\n"
  sol <- unionSolve (Map.empty, cs ++ cons)
  let tpWithKinds = mapKind (apply sol) tp
  case Map.lookup name sol of
    Just a -> return (a, tpWithKinds)
    Nothing -> throwError $ DoesNotAppear name

union :: Kind -> Kind -> Constraints
union a b = [(a, b)]

generateConstraints :: Type -> InferKind (Constraints, Kind, Type)
generateConstraints = \case
  TVar (TV a _) -> do
    (k, t) <- lookupEnv a
    return ([], k, t)
  TCon s k -> return ([], k, TCon s k)
  TApp a b -> do
    ret <- fresh
    (consA, ka, ta) <- generateConstraints a
    (consB, kb, tb) <- generateConstraints b
    return (consA ++ consB ++ union ka (Kfun kb ret), ret, TApp ta tb)

type Subst = Map.Map String Kind

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

applyT :: Subst -> (Kind, Kind) -> (Kind, Kind)
applyT s (a, b) = (apply s a, apply s b)

apply :: Subst -> Kind -> Kind
apply s = \case
  Kfun a b -> Kfun (apply s a) (apply s b)
  k@(KVar a) -> Map.findWithDefault k a s
  k -> k

type Unifier = (Subst, Constraints)

unionSolve :: Unifier -> InferKind Subst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    (t1, t2) : cs1 -> do
      su1 <- unifies t1 t2
      unionSolve (su1 `compose` su, applyT su1 <$> cs1)

unifies :: Kind -> Kind -> InferKind Subst
unifies t1 t2 | t1 == t2 = return Map.empty
unifies (KVar v) t = bind v t
unifies t (KVar v) = bind v t
unifies (Kfun k1 k2) (Kfun k1' k2') =  do
  s1 <- unifies k1 k1'
  s2 <- unifies k2 k2'
  return $ s2 `compose` s1
unifies k1 k2 = throwError $ KindUnificationFail k1 k2

bind :: String -> Kind -> InferKind Subst
bind a t | t == KVar a = return Map.empty
         | otherwise = return $ Map.singleton a t
