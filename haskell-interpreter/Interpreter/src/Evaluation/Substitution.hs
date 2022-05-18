module Evaluation.Substitution where

import Syntax.Expression
import Data.Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars expr = elems $ freeVars' expr

freeVars' :: Expression -> Set String
freeVars' (Variable var) = singleton var
freeVars' (Function var expr) = freeVars' expr `difference` singleton var
freeVars' (Application lhs rhs) = freeVars' lhs `union` freeVars' rhs
freeVars' (Declaration _ expr) = freeVars' expr

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst str new v@(Variable var)
    | str == var = new
    | otherwise  = v
subst str new f@(Function var expr)
    | str == var = f
    | str /= var && var `notElem` freeVars new = Function var $ subst str new expr
    | otherwise = Function (var ++ "#") (subst str new (subst var (Variable $ var ++ "#") expr))
subst str new (Application lhs rhs) = Application (subst str new lhs) (subst str new rhs)
subst str new (Declaration name expr) = Declaration name (subst str new expr)

