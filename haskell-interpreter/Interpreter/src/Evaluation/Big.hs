module Evaluation.Big where

import Syntax.Expression
import Control.Monad.State
import Control.Monad.Except

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.

    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition

evalBig evalF expr = runState (evalBigM (state . evalF) expr)

type Eval = State Context

evalBigM :: (Expression -> Eval Expression) -> Expression -> Eval Expression
-- evalBigM evalF expr = evalF expr >>= \expr' -> if expr == expr' then return expr else evalBigM evalF expr'
evalBigM evalF expr = do
    expr' <- evalF expr
    if expr == expr'
        then return expr
        else evalBigM evalF expr'

{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.

    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)

evalList evalF exprs = runState (evalListM (state . evalF) exprs)

evalListM :: (Expression -> Eval Expression) -> [Expression] -> Eval [Expression]
evalListM = mapM . evalBigM

type EvalE = StateT Context (ExceptT String IO)

evalBig' :: (Expression -> EvalE Expression)
         -> Expression
         -> Context
         -> IO (Either String (Expression, Context))
evalBig' evalF expr ctx = runExceptT (f `catchError` \s -> liftIO (putStrLn s) >> f)
    where f = runStateT (evalBigE evalF expr) ctx

evalBigE :: (Expression -> EvalE Expression) -> Expression -> EvalE Expression
evalBigE evalF expr = do
    expr' <- evalF expr
    if expr == expr'
       then return expr
       else evalBigE evalF expr'

evalList' :: (Expression -> EvalE Expression)
          -> [Expression]
          -> Context
          -> IO (Either String ([Expression], Context))
evalList' evalF exprs ctx = runExceptT (f `catchError` \s -> liftIO (putStrLn s) >> f)
    where f = runStateT (evalListE evalF exprs) ctx

evalListE :: (Expression -> EvalE Expression) -> [Expression] -> EvalE [Expression]
evalListE = mapM . evalBigE


