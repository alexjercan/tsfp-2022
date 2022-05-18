module Syntax.Expression where

import Data.Map as M

data Expression = Declaration String Expression
                | Variable String
                | Function String Expression
                | Application Expression Expression

type Context = M.Map String Expression

instance Show Expression where
    show (Declaration name expr) = name ++ "=" ++ show expr
    show (Variable name) = name
    show (Function name expr) = "\\" ++ name ++ "." ++ show expr
    show (Application lhs rhs) = "(" ++ show lhs ++ " " ++ show rhs ++ ")"

instance Eq Expression where
    (==) (Declaration name1 expr1) (Declaration name2 expr2) = name1 == name2 && expr1 == expr2
    (==) (Variable name1) (Variable name2) = name1 == name2
    (==) (Function name1 expr1) (Function name2 expr2) = name1 == name2 && expr1 == expr2
    (==) (Application lhs1 rhs1) (Application lhs2 rhs2) = lhs1 == lhs2 && rhs1 == rhs2
    (==) _ _ = False

