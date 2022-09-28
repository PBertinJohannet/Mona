{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Dispatch where
import RecursionSchemes
import Syntax
import Type
import Pretty
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Data.Maybe
import qualified Data.Map as Map
import Env
import Subst
import Error
import Data.List

type DispatchReq = (String, TExpr, Subst)

data DispatchState = DState {dispatched :: Map.Map String TExpr, todispatch :: [DispatchReq]}

baseState :: DispatchState
baseState = DState Map.empty []

addTarget :: String -> TExpr -> Subst -> DispatchState -> DispatchState
addTarget name ex sub (DState d t) = DState d $ t ++ [(name, ex, sub)]

next :: String -> TExpr -> Subst -> Map.Map String TExpr -> Dispatch ()
next name ex sub disp = case Map.lookup name disp of
  Just already -> do
    tell $ name ++ " already dispatched"
    return ()
  Nothing -> do
    tell $ "sub is : " ++ pretty sub ++ "\n"
    tell $ "dispatching : " ++ name ++ "\n"
    tell $ "before : " ++ pretty ex ++ "\n"
    tell $ "after : " ++ pretty (apply sub ex) ++ "\n"
    ex <- dispatch $ apply sub ex
    modify $ register name ex

dispatchAll :: Dispatch ()
dispatchAll = do
  (DState disp todisp) <- get
  case todisp of
    (name, ex, sub) : remain -> do
      put $ DState disp remain
      next name ex sub disp
      dispatchAll
    _ -> return ()

register :: String -> TExpr -> DispatchState -> DispatchState
register name tx (DState disp todisp) = DState (Map.insert name tx disp) todisp

{-
next :: Dispatch (Maybe (Subst, String))
next = do
  (DState disp (nxt:_)) <- get
  dispatch nxt
  _ok
-}

data DispatchError
  = MainNotFound

instance Pretty DispatchError where
  pretty = \case
    MainNotFound -> "Main not found"

type Dispatch a = ExceptT DispatchError (RWS Envs String DispatchState) a

toWriter :: (Either DispatchError TAst, String) -> ExceptT DispatchError (Writer String) TAst
toWriter (a, b) = do
  tell b
  (throwError ||| return) a

runDispatch :: Envs -> ExceptT DispatchError (Writer String) TAst
runDispatch envs = toWriter $ evalRWS (runExceptT (dispatch $ ast envs)) envs baseState

class Dispatchable a where
  dispatch :: a -> Dispatch a

instance Dispatchable TAst where
  dispatch (TAst env cmp) = do
    case Map.lookup "main" env of
      Nothing -> do
        tell $ "did not found main in : " ++ mconcat (intersperse "\n" (pretty <$> Map.toList env))
        throwError MainNotFound
      Just ex -> do
        ex <- dispatch ex
        modify $ register "main" ex
    dispatchAll
    DState done _ <- get
    return $ TAst done cmp

instance Dispatchable TExpr where
  dispatch ex= do
    res <- cataCF dispatchAlg ex
    (DState disp todisp) <- get
    tell $ "state is now : " ++ keysK (TAst disp Map.empty) ++ "\n"
    tell $ "with reqs : " ++ show (length todisp) ++ "\n"
    return res

findInExprs :: String -> [Pred] -> Dispatch (Maybe (TExpr, String))
findInExprs n preds = do
  (Envs _ _ _ (TAst env _)) <- ask
  case Map.lookup n env of
    Just vl -> return $ Just (vl, n)
    Nothing -> case preds of
      IsIn _ tp : _ -> do
        let name = n ++ " " ++ pretty tp
        tell $ "searching for : " ++ name ++ "\n"
        tell $ "in " ++ show (fst <$> Map.toList env) ++ "\n"
        return $ (,name) <$> Map.lookup name env
      _ -> return Nothing

dispatchAlg :: ((Location, Subst, Qual Type), ExprF (Dispatch TExpr)) -> Dispatch TExpr
dispatchAlg = \case
  ((loc, sub, Qual p tp), Var vr) -> do
    --tell $ "sub is " ++ pretty sub ++ "\n"
    found <- findInExprs vr p
    res <- case found of
      Just (ex, v) -> do
        let v = vr ++ pretty tp
        tell $ "request dispatch of " ++ vr ++ " as " ++ v ++ "\n"
        tell $ "body " ++ pretty ex ++ "\n"
        modify $ addTarget v ex sub
        return v
      Nothing -> do
        tell $ "no dispatch for : " ++ vr ++ "\n"
        return vr
    return $ In $ (loc, sub, Qual p tp) :< Var res
  (a, b) -> do
    b <- sequence b
    return $ In $ a :< b
