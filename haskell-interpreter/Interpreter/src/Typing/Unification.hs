module Typing.Unification where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Typing.Type

{-
    A monad for solving unification constraints. It is composed of:
    * a state monad transformer, for maintaining the substitution
    * an exception monad, for signaling unification errors.
-}
type Unif = StateT Substitution (Except String)

runUnif :: Unif a -> Substitution -> Either String (a, Substitution)
runUnif ops subst = runExcept $ runStateT ops subst

{-|
    Obtains the end of the binding chain for the given type.
    The search ends when either of the following is reached:

    * an unbound type variable

    * a function type.
-}
chainEnd ::
       Type -- ^ Type to look up
    -> Unif Type -- ^ Chain end
chainEnd a@(Arrow _ _) = return a
chainEnd t@(TypeVar s) = do
    sm <- get
    case sm M.!? s of
        Nothing -> return t
        Just t' -> chainEnd t'

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck ::
       String -- ^ Type variable to check for occurrence
    -> Type -- ^ Type to look in
    -> Unif Bool -- ^ True if the type variable does NOT occur
occCheck s t = do
    t' <- chainEnd t
    case t' of
        Arrow a b -> (&&) <$> occCheck s a <*> occCheck s b
        TypeVar v -> return $ s /= v

{-|
    Unifies two type expressions.
-}
unify ::
       Type -- ^ First type
    -> Type -- ^ Second type
    -> Unif () -- ^ () if the types unify or an exception otherwise
unify t1 t2 = do
    t1' <- chainEnd t1
    t2' <- chainEnd t2
    unify' t1' t2'

unify' :: Type -> Type -> Unif ()
unify' (TypeVar a) b'@(TypeVar b) = when (a /= b) (modify $ M.insert a b')
unify' (TypeVar a) b'@(Arrow _ _) = do
    occ <- occCheck a b'
    asd <- get
    if occ
        then modify $ M.insert a b'
        else throwError $ "occCheck " ++ a ++ " " ++ show b' ++ " " ++ show asd
unify' a'@(Arrow _ _) b'@(TypeVar _) = unify' b' a'
unify' (Arrow a1 a2) (Arrow b1 b2) = unify' a1 b1 >> unify a2 b2

{-|
    Applies the substitution to a type expression.
-}
applySubst ::
       Type -- ^ Target type
    -> Unif Type -- ^ Resulting type
applySubst (Arrow a b) = Arrow <$> applySubst a <*> applySubst b
applySubst t@(TypeVar s) = do
    sm <- get
    case sm M.!? s of
        Nothing -> return t
        Just t' -> applySubst t'
