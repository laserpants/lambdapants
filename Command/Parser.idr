module Command.Parser

import Command
import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Term
import Term.Parser

alphaCompare : Parser (Either String Command)
alphaCompare = do
  string "alpha" 
  spaces
  (eof *> usage) <|> arg <|> pure (Left "bananas")
where
  usage : Parser (Either String Command)
  usage = pure (Left "Usage is :alpha <term> <term>")
  arg : Parser (Either String Command)
  arg = do
    t <- term 
    case t of
         (App a b) => pure (Right (AlphaCompare a b))
         otherwise => usage
