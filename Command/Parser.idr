module Command.Parser

import Command
import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Term
import Term.Parser

help : Parser (Either String Command)
help = ?help

env : Parser (Either String Command)
env = ?env

aq : Parser (Either String Command)
aq = do
  string "aq" 
  spaces
  (eof *> usage) <|> arg <|> usage
where
  usage : Parser (Either String Command)
  usage = pure (Left "Usage is :alpha <term> <term>")
  arg : Parser (Either String Command)
  arg = do
    t <- term 
    case t of
         (App a b) => pure (Right (AlphaCompare a b))
         otherwise => usage

eq : Parser (Either String Command)
eq = ?eq

reduce : Parser (Either String Command)
reduce = ?reduce

lookup : Parser (Either String Command)
lookup = ?lookup

save : Parser (Either String Command)
save = ?save

delete : Parser (Either String Command)
delete = ?delete

limit : Parser (Either String Command)
limit = ?limit

quit : Parser (Either String Command)
quit = ?quit
