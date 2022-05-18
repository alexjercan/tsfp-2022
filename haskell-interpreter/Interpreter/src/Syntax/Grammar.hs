module Syntax.Grammar where

import Syntax.Expression
import Syntax.Parser
import Control.Applicative
import Data.Char

ws :: Parser ()
ws = () <$ some (spot isSpace) <|> eof

variableP :: Parser Expression
variableP = Variable <$> some letter

functionP :: Parser Expression
functionP = Function <$> (token '\\' *> some letter <* token '.') <*> expressionP

applicationP :: Parser Expression
applicationP = Application <$> (token '(' *> expressionP <* ws)
                           <*> (expressionP <* token ')')

declarationP :: Parser Expression
declarationP = Declaration <$> (some letter <* token '=') <*> (functionP <|> applicationP <|> variableP)

expressionP :: Parser Expression
expressionP = declarationP <|> variableP <|> functionP <|> applicationP

manyEndWith :: Parser a -> Parser b -> Parser [b]
manyEndWith end element = some (element <* end) <|> pure []

parseProgram :: String -> Maybe [Expression]
parseProgram input = fst <$> runParser (manyEndWith ws expressionP) input
