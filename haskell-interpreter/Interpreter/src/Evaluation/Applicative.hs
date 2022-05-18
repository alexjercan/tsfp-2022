module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import Control.Monad.State
import Data.Functor
import qualified Data.Map as M

{-|
    Small-step applicative-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval expr = runState (evalM expr)
{-
eval (Declaration name expr) ctx = (expr, M.insert name expr ctx)

eval (Application (Function name expr) f@(Function _ _)) ctx = (subst name f expr, ctx)
eval (Application f@(Function _ _) expr) ctx = (Application f (fst (eval expr ctx)), ctx)
eval (Application lhs rhs) ctx = (Application (fst (eval lhs ctx)) rhs, ctx)

eval v@(Variable name) ctx = (M.findWithDefault v name ctx, ctx)
eval f@(Function _ _) ctx = (f, ctx)
-}

type Eval = State Context

evalM :: Expression -> Eval Expression
evalM (Declaration name expr) = withState (M.insert name expr) (return expr)
evalM (Application (Function name expr) f@(Function _ _)) = return $ subst name f expr
evalM (Application f@(Function _ _) expr) = evalM expr <&> Application f
evalM (Application lhs rhs) = evalM lhs <&> (`Application` rhs)
evalM v@(Variable name) = get <&> M.findWithDefault v name
evalM f@(Function _ _) = return f

