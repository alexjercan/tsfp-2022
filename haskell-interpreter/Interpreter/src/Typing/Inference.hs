module Typing.Inference where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (Bifunctor(second))
import qualified Data.Map as M
import Syntax.Expression
import Typing.Type
import Typing.Unification

{-|
    The type of inference state.

    Should comprise:

    * The global typing context

    * The type variable counter.
-}
data TypingState =
    TypingState
        { context :: TypingContext
        , counter :: Counter
        }
    deriving (Show)

{-|
    The type of the inference mechanism.

    Should expose the following:

    * Access to the inference state (State)

    * Acces to the local typing context (Reader)

    * A means for storing unification constraints (Writer)
-}
type Infer = ReaderT TypingContext (WriterT [(Type, Type)] (State TypingState))

runInfer ::
       Infer a -- ^ Expression to type
    -> TypingContext -- ^ Local context
    -> TypingContext -- ^ Global context
    -> Counter -- ^ Current type variable counter
    -> (a, [(Type, Type)])
                           -- ^ The result, along with possible unification
                           --   constraints; otherwise, an error message
runInfer inf loc glob cnt =
    evalState (runWriterT $ runReaderT inf loc) $ TypingState glob cnt

{-|
    Generates a copy of the given type.

    Should rely on 'copyM' below.
-}
copy :: Type -> Type
copy t = fst $ runInfer (copyM t) M.empty M.empty 0

{-|
    The type inference function, wich synthesizes the type of the given
    expression.

    Should rely on 'inferM' below.
-}
infer ::
       Expression -- ^ Expression to type
    -> TypingContext -- ^ Local context
    -> TypingContext -- ^ Global context
    -> Substitution -- ^ Substitution
    -> Counter -- ^ Current type variable counter
    -> Either String Type -- ^ If the typing succeeds,
                             --   the inferred type; otherwise, an error
                             --   message.
infer expr loc glob subst cnt =
    case runUnif (mapM_ (uncurry unify) cs) subst of
        Right (_, subst') -> second fst $ runUnif (applySubst t) subst'
        Left left -> Left left
  where
    (t, cs) = runInfer (inferM expr) loc glob cnt

{-|
    Generates a new type variable using the counter hidden within the state,
    and updates the latter.
-}
newTypeVar :: Infer Type
newTypeVar = do
    cnt <- gets counter
    modify (\x -> x {counter = cnt + 1})
    return $ TypeVar ("t" ++ show cnt)

{-|
    See 'copy'.
        -}
copyM :: Type -> Infer Type
copyM (TypeVar a) = do
    loc <- ask
    maybe newTypeVar return (loc M.!? a)
copyM (Arrow t1@(TypeVar a) t2) = do
    t <- copyM t1
    Arrow t <$> local (M.insert a t) (copyM t2)
copyM (Arrow t1 t2) = Arrow <$> copyM t1 <*> copyM t2
    {-
copyM :: Type -> Infer Type
copyM t = evalStateT (copyM' t) M.empty
-}

copyM' :: Type -> StateT Substitution Infer Type
copyM' t@(TypeVar a) = undefined
copyM' (Arrow t1 t2) = undefined

{-|
    See 'infer'.
-}
inferM :: Expression -> Infer Type
inferM (Variable a) = do
    loc <- ask
    case loc M.!? a of
        Nothing -> do
            glob <- gets context
            case glob M.!? a of
                Nothing -> newTypeVar
                Just t' -> do
                    t'' <- copyM t'
                    tell [(t', t'')]
                    return t''
        Just t -> return t
inferM (Function a b) = do
    t <- newTypeVar
    Arrow t <$> local (M.insert a t) (inferM b)
inferM (Application lhs rhs) = do
    t1 <- inferM lhs
    t2 <- inferM rhs
    t3 <- newTypeVar
    tell [(t1, Arrow t2 t3)]
    return t3
inferM (Declaration a b) = do
    glob <- gets context
    t <- newTypeVar
    modify (\x -> x {context = M.insert a t glob})
    t' <- local (M.insert a t) (inferM b)
    tell [(t, t')]
    return t'
