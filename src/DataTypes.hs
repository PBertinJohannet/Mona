{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataTypes where
import Pretty
import Syntax
import Env (lookup, KEnv(..), Envs(..), letters, extends, extend, withCompiled)
import Type
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe
import Data.List (intercalate, find)
import RecursionSchemes
import Native
import Infer (InferState(..), initInfer)
import Run (Value(..), Run, makeRunPat, makeRunCons)
import Subst (withFtv)
import Error

type DataDeclError = CompilerError DataDeclErrorV

data DataDeclErrorV
 = KindUnificationFail Kind Kind
 | InfiniteKind Kind
 | WrongReturnType String Type Type deriving (Show, Eq)

instance Pretty DataDeclErrorV where
  pretty = \case
    InfiniteKind k -> "(InfiniteKind) Could not construct the kind " ++ pretty k
    KindUnificationFail a b -> "(KindUnificationFail) Could not unify kinds " ++ pretty a ++ " and  " ++ pretty b
    WrongReturnType a b c -> "(WrongReturnType) signature's return type should be an application of " ++ a ++ " but it is an application of " ++ pretty c ++ " (in : " ++ pretty b ++ ")"


runDataDecls :: [DataDecl] -> Envs -> ExceptT DataDeclError (Writer String) Envs
runDataDecls ds env = foldM runDataDecl env ds

runDataDecl :: Envs-> DataDecl -> ExceptT DataDeclError (Writer String) Envs
runDataDecl envs@(Envs d v cenv tast) (loc, name, tvars, types) = do
  (kind, cons) <- runInferKind d $ inferKinds name tvars (snd <$> types) `withErrorLoc` loc
  tell $ "for "  ++ name ++ " cons are : " ++ prettyL cons
  let typeList = asList types
  let consSchemes = makeCons (fst <$> typeList) name cons
  let patsSchemes = consToPat <$> consSchemes
  let v' = foldr (flip extend) v $ consSchemes ++ patsSchemes
  let runCons = makeRunCons <$> zip3 (lenMinusOne . sepArgs . snd <$> typeList) [0..] (fst <$> typeList)
  let runPats = makeRunPat <$> zip [0..] (fst <$> typeList)
  let tast' = withCompiled tast $ runPats ++ runCons
  return $ Envs (extend d (name, kind)) v' cenv tast'


makeCons :: [String] -> String -> [Type] -> [(String, Scheme)]
makeCons consNames tpName types = do
  (n, t) <- zip consNames types
  let a = makeTypeConstant tpName t
  return (n, withFtv a)

inferKinds :: String -> [String] -> NonEmpty Type -> InferKind (Kind, [Type])
inferKinds name tvars tps = do
  res <- makeBaseEnv name tvars
  kinds@((k, t) :+: ks) <- mapM (inferConstraints res name) tps
  mapM_ (checkEndsWith name) tps
  let res = mconcat (union k . fst <$> ks)
  sub <- unionSolve (Map.empty, res)
  let typesWithKinds =  mapKind (apply sub) . snd <$> kinds
  return (apply sub k, asList typesWithKinds)

type InferKind = ReaderT KEnv (StateT InferState (ExceptT DataDeclError (Writer String)))

runInferKind :: KEnv -> InferKind a -> ExceptT DataDeclError (Writer String) a
runInferKind kenv inf = evalStateT (runReaderT inf kenv) initInfer

-- custom state monad with a Map.Map String String inside.
instantiate :: Kind -> InferKind Kind
instantiate k = snd <$> replaceAll Map.empty k
  where
    replaceAll :: Map.Map String String -> Kind -> InferKind (Map.Map String String, Kind)
    replaceAll sub = \case
      KVar a -> second KVar <$> addTo a sub
      Star -> return (sub, Star)
      Kfun k1 k2 -> do
        (s0, k1') <- replaceAll sub k1
        (s1, k2') <- replaceAll s0 k2
        return (s1, Kfun k1' k2')
    addTo :: String -> Map.Map String String -> InferKind (Map.Map String String, String)
    addTo s m = case Map.lookup s m of
      Just k -> return (m, k)
      Nothing -> do
        tv <- freshV
        return (Map.insert s tv m, tv)

lookupEnv :: String -> InferKind (Kind, Type)
lookupEnv s = do
  res <- Env.lookup s <$> ask
  case res of
    Just a -> do
      ret <- instantiate a
      return (ret, TCon s ret)
    Nothing -> return (KVar s, TVar $ TV s (KVar s))

fresh :: InferKind Kind
fresh = KVar <$> freshV

freshV :: InferKind String
freshV = do
  s <- get
  put s{count = count s + 1}
  return (letters !! count s)


makeBaseEnv :: String -> [String] -> InferKind Constraints
makeBaseEnv name [] = return [(KVar name, Star)]
makeBaseEnv name (tvar:tvars) = do
  a <- freshV
  next <- makeBaseEnv a tvars
  --tell $ "adding : " ++ pretty (name, Kfun (KVar tvar) (KVar a))
  return ((KVar ("''" ++ name), Kfun (KVar tvar) (KVar a)):next)

type Constraints = [(Kind, Kind)]

instance Pretty (Kind, Kind) where
  pretty (a, b) = pretty a ++ " <=> " ++ pretty b

checkEndsWith :: String -> Type -> InferKind ()
checkEndsWith name t = checkIs $ leftMostType $ lastSafe $ sepArgs t
  where
    checkIs :: Type -> InferKind ()
    checkIs (TCon a _) | a == name = return ()
    checkIs (TVar (TV a _)) | a == name = return ()
    checkIs e = throwErrorV $ WrongReturnType name t e

inferConstraints :: Constraints -> String -> Type -> InferKind (Kind, Type)
inferConstraints cs name t = do
  env <- ask
  tell $ "\n===============" ++ name ++ "==============\n"
  --tell $ "\n base : \n" ++ pretty env
  (cons, _, tp) <- generateConstraints t
  --tell $ pretty t
  tell $ "cons found : " ++ show (fmap pretty (cs ++ cons)) ++ "\n"
  sol <- unionSolve (Map.empty, cs ++ cons)
  let tpWithKinds = mapKind (apply sol) tp
  case Map.lookup name sol of
    Just a -> return (a, tpWithKinds)
    Nothing -> throwErrorV $ WrongReturnType name t t

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
      --tell $ "\n\nunifying : " ++ pretty t1 ++ " and " ++ pretty t2 ++ "\n"
      su1 <- unifies t1 t2
      --tell $ "found : " ++ prettyM su1
      unionSolve (su1 `compose` su, applyT su1 <$> cs1)

unifies :: Kind -> Kind -> InferKind Subst
unifies t1 t2 | t1 == t2 = return Map.empty
unifies (KVar v) t = bind v t
unifies t (KVar v) = bind v t
unifies (Kfun k1 k2) (Kfun k1' k2') =  do
  --tell $ "going down with : " ++ pretty k1 ++ " == " ++ pretty k1' ++ "\n"
  --tell $ "and : " ++ pretty k2 ++ " == " ++ pretty k2' ++ "\n"
  s1 <- unifies k1 k1'
  tell $ "found " ++ prettyM s1 ++ "\n"
  --tell $ "second became : " ++ pretty k2 ++ " == " ++ pretty k2' ++ "\n"
  s2 <- unifies (apply s1 k2) (apply s1 k2')
  tell $ "found " ++ prettyM s2 ++ "\n"
  return $ s2 `compose` s1
unifies k1 k2 = throwErrorV $ KindUnificationFail k1 k2

bind :: String -> Kind -> InferKind Subst
bind a k | k == KVar a = return Map.empty
         | occursCheck a k = throwErrorV $ InfiniteKind k
         | otherwise = return $ Map.singleton a k

occursCheck :: String -> Kind -> Bool
occursCheck a k = a `Set.member` getVars k
  where
    getVars = \case
      KVar a -> Set.singleton a
      Star -> Set.empty
      Kfun k1 k2 -> getVars k1 `Set.union` getVars k2
