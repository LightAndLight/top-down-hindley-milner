{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Check (check, infer, CheckT, runCheckT, Error (..), freshMeta) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks, local)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Core
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Any (..))
import Syntax (Span, Spanned)
import qualified Syntax
import Type (Type)
import qualified Type
import Prelude hiding (span)

data Solution = Unsolved | Solved Type
  deriving (Eq, Show)

newtype CheckT m a = CheckT (ReaderT (Map String Type.Scheme) (StateT (IntMap Solution) (ExceptT Error m)) a)
  deriving (Functor, Applicative, Monad, MonadError Error)

runCheckT :: Monad m => Map String Type.Scheme -> CheckT m a -> m (Either Error a)
runCheckT ctx (CheckT m) = runExceptT (evalStateT (runReaderT m ctx) mempty)

data Error
  = NotInScope {span :: Span}
  | TypeMismatch {span :: Span, expected :: Type, actual :: Type}
  | Occurs {span :: Span, meta :: Int, ty :: Type}
  deriving (Eq, Show)

freshMeta :: Monad m => CheckT m Type
freshMeta =
  CheckT $ do
    n <- gets IntMap.size
    modify $ IntMap.insert n Unsolved
    pure $ Type.Meta n

lookupName :: Monad m => String -> CheckT m (Maybe Type.Scheme)
lookupName meta =
  CheckT . asks $ Map.lookup meta

lookupSolution :: Monad m => Int -> CheckT m Solution
lookupSolution meta =
  CheckT . gets $
    Maybe.fromMaybe (error $ "meta " <> show meta <> " not found")
      . IntMap.lookup meta

setSolution :: Monad m => Int -> Type -> CheckT m ()
setSolution meta ty =
  CheckT . modify $ IntMap.insert meta (Solved ty)

withName :: Monad m => String -> Type.Scheme -> CheckT m a -> CheckT m a
withName name ty (CheckT m) =
  CheckT $ local (Map.insert name ty) m

walk :: Monad m => Type -> CheckT m Type
walk ty =
  case ty of
    Type.Meta meta -> do
      solution <- lookupSolution meta
      case solution of
        Unsolved ->
          pure ty
        Solved ty' ->
          walk ty'
    _ ->
      pure ty

checkOccurs :: Monad m => Int -> Type -> CheckT m (Type, Bool)
checkOccurs meta ty = do
  (ty', Any occurs) <- runWriterT $ go ty
  pure (ty', occurs)
 where
  go :: Monad m => Type -> WriterT Any (CheckT m) Type
  go ty' =
    case ty' of
      Type.Var{} ->
        pure ty'
      Type.Arrow inTy outTy -> do
        Type.Arrow <$> go inTy <*> go outTy
      Type.Meta meta' -> do
        tell $ Any (meta == meta')
        solution <- lift (lookupSolution meta')
        case solution of
          Unsolved ->
            pure ty'
          Solved ty'' ->
            go ty''

solveL :: Monad m => UnifyContext -> Int -> Type -> CheckT m Type
solveL ctx meta ty = do
  (ty', occurs) <- checkOccurs meta ty
  when occurs $ throwError Occurs{span = ctx.span, meta, ty = ty'}
  solution <- lookupSolution meta
  case solution of
    Unsolved ->
      ty' <$ setSolution meta ty'
    Solved expected ->
      unify ctx expected ty'

solveR :: Monad m => UnifyContext -> Type -> Int -> CheckT m Type
solveR ctx ty meta = do
  (ty', occurs) <- checkOccurs meta ty
  when occurs $ throwError Occurs{span = ctx.span, meta, ty = ty'}
  solution <- lookupSolution meta
  case solution of
    Unsolved ->
      ty' <$ setSolution meta ty'
    Solved actual ->
      unify ctx ty' actual

zonk :: Monad m => Type -> CheckT m Type
zonk ty =
  case ty of
    Type.Meta meta -> do
      solution <- lookupSolution meta
      case solution of
        Unsolved ->
          pure ty
        Solved ty' ->
          zonk ty'
    Type.Arrow a b ->
      Type.Arrow <$> zonk a <*> zonk b
    Type.Var{} ->
      pure ty

typeMismatch :: Monad m => Span -> Type -> Type -> CheckT m a
typeMismatch span e a = do
  expected <- zonk e
  actual <- zonk a
  throwError TypeMismatch{span, expected, actual}

data UnifyContext = UnifyContext {span :: Span, expected :: Type, actual :: Type}

unify :: Monad m => UnifyContext -> Type -> Type -> CheckT m Type
unify ctx a b = do
  a' <- walk a
  b' <- walk b
  go a' b'
 where
  go expected actual =
    case expected of
      Type.Meta meta ->
        case actual of
          Type.Meta meta'
            | meta == meta' ->
                pure expected
          _ ->
            solveL ctx meta actual
      Type.Var name ->
        case actual of
          Type.Meta meta ->
            solveR ctx expected meta
          Type.Var name' ->
            if name == name'
              then pure expected
              else typeMismatch ctx.span ctx.expected ctx.actual
          _ ->
            typeMismatch ctx.span ctx.expected ctx.actual
      Type.Arrow inTy outTy ->
        case actual of
          Type.Meta meta ->
            solveR ctx expected meta
          Type.Arrow inTy' outTy' ->
            Type.Arrow
              <$> unify ctx inTy inTy'
              <*> unify ctx outTy outTy'
          _ ->
            typeMismatch ctx.span ctx.expected ctx.actual

subst :: [(String, Type)] -> Type -> Type
subst vars ty =
  case ty of
    Type.Var name ->
      case lookup name vars of
        Nothing ->
          ty
        Just ty' ->
          ty'
    Type.Arrow a b ->
      Type.Arrow (subst vars a) (subst vars b)
    Type.Meta{} ->
      ty

instantiate :: Monad m => Type.Scheme -> CheckT m Type
instantiate (Type.Scheme vars ty) = do
  vars' <- traverse (\var -> (,) var <$> freshMeta) vars
  pure $ subst vars' ty

check :: Monad m => Spanned Syntax.Expr -> Type -> CheckT m (Core.Expr, Type)
check expr expected =
  case expr.value of
    Syntax.Var name -> do
      actual <-
        instantiate
          =<< maybe (throwError $ NotInScope expr.span) pure
          =<< lookupName name

      ty <- unify UnifyContext{span = expr.span, expected, actual} expected actual

      pure (Core.Var name, ty)
    Syntax.Lam name body -> do
      inTy <- freshMeta
      outTy <- freshMeta

      let actual = Type.Arrow inTy outTy
      _ <- unify UnifyContext{span = expr.span, expected, actual} expected actual

      (body', outTy') <- withName name (Type.Scheme mempty inTy) $ check body outTy

      inTy' <- walk inTy
      pure (Core.Lam name inTy' body', Type.Arrow inTy' outTy')
    Syntax.App f x -> do
      inTy <- freshMeta
      outTy <- freshMeta

      (f', _) <- check f $ Type.Arrow inTy outTy
      (x', _) <- check x inTy

      ty <- unify UnifyContext{span = expr.span, expected, actual = outTy} expected outTy
      pure (Core.App f' x', ty)
    Syntax.With name ty rest ->
      withName name ty $ check rest expected
    Syntax.String str -> do
      let actual = Type.Var "String"
      actual' <- unify UnifyContext{span = expr.span, expected, actual} expected actual
      pure (Core.String str, actual')
    Syntax.Int int -> do
      let actual = Type.Var "Int"
      actual' <- unify UnifyContext{span = expr.span, expected, actual} expected actual
      pure (Core.Int int, actual')
    Syntax.IfThenElse cond then_ else_ -> do
      (cond', _) <- check cond $ Type.Var "Bool"
      (then_', ty) <- check then_ expected
      (else_', ty') <- check else_ ty

      pure (Core.IfThenElse cond' then_' else_', ty')

infer :: Monad m => Spanned Syntax.Expr -> CheckT m (Core.Expr, Type)
infer expr =
  freshMeta >>= check expr