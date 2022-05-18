module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import Control.Monad.State
import Data.Functor
import qualified Data.Map as M
import Control.Monad.Except

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval expr = runState (evalM expr)

type Eval = State Context

evalM :: Expression -> Eval Expression
evalM (Declaration name expr) = withState (M.insert name expr) (return expr)
evalM (Application (Function name expr) rhs) = return $ subst name rhs expr
evalM (Application lhs rhs) = evalM lhs <&> (`Application` rhs)
evalM v@(Variable name) = gets $ M.findWithDefault v name
evalM f@(Function _ _) = return f

eval' :: Expression -> Context -> IO (Either String (Expression, Context))
eval' expr ctx = runExceptT (f `catchError` \s -> liftIO (putStrLn s) >> f)
    where f = runStateT (evalE expr) ctx

-- transformers order of "application" is from left to right (out to inner)
type EvalE = StateT Context (ExceptT String IO)

evalE :: Expression -> EvalE Expression
evalE (Declaration name expr) = modify (M.insert name expr) >> return expr
evalE (Application (Function name expr) rhs) = return $ subst name rhs expr
evalE (Application lhs rhs) = evalE lhs <&> (`Application` rhs)
evalE (Variable name) = do
    ctx <- get
    case ctx M.!? name of
      Just e  -> return e
      Nothing -> throwError $ "Variable " ++ name ++ " not found"
evalE f@(Function _ _) = return f

