module Command.Parser

import Command
import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Term
import Term.Parser

termArg : Parser (Maybe Term)
termArg = (spaces *> eof *> pure Nothing) <|> (some space *> map Just term)

export
help : Parser Command
help = do
  char '?' <|> (char 'h' <* opt (string "elp"))
  spaces
  eof
  pure Help

export
env : Parser Command
env = do
  string "env"
  (spaces *> eof *> pure (Env Nothing)) <|> arg
where
  arg : Parser Command
  arg = do
    some space
    term <- atom
    pure (Env (Just term))

export
aq : Parser (Either String Command)
aq = do
  string "aq"
  t <- termArg
  case t of
       Just (App s t) => pure (Right (AlphaEq s t))
       otherwise      => pure (Left "Usage is :aq <term> <term>")

export
eq : Parser (Either String Command)
eq = do
  string "eq"
  t <- termArg
  case t of
       Just (App s t) => pure (Right (Eq s t))
       otherwise      => pure (Left "Usage is :eq <term> <term>")

export
reduce : Parser (Either String Command)
reduce = do
  char 'r'
  opt (string "educe")
  t <- termArg
  case t of
       Just t    => pure (Right (Reduce t))
       otherwise => pure (Left "Usage is :r[educe] <term>")

export
lookup : Parser (Either String Command)
lookup = do
  char 'l'
  opt (string "ookup")
  t <- termArg
  case t of
       Just t    => pure (Right (Lookup t))
       otherwise => pure (Left "Usage is :l[ookup] <term>")

export
save : Parser (Either String Command)
save = do
  char 's'
  opt (string "ave")
  (spaces *> eof *> usage) <|> (some space *> (args <|> usage))
where
  usage : Parser (Either String Command)
  usage = pure (Left "Usage is :s[ave] <symbol> <term>")
  args : Parser (Either String Command)
  args = do
    symb <- atom
    expr <- termArg
    case expr of
         Just term => pure (Right (Save symb term))
         otherwise => usage

export
delete : Parser (Either String Command)
delete = do
  char 'd'
  opt (string "elete")
  (spaces *> eof *> pure (Left "Usage is :d[elete] <symbol>")) <|> arg
where
  arg : Parser (Either String Command)
  arg = some space *> map (Right . Delete) atom

export
limit : Parser (Either String Command)
limit = do
  string "limit"
  (spaces *> eof *> usage) <|> (some space *> (arg <* eof) <|> usage)
where
  usage : Parser (Either String Command)
  usage = pure (Left "Usage is :limit <number>")
  arg : Parser (Either String Command)
  arg = (Right . Limit . cast . pack) <$> some (satisfy isDigit)

export
quit : Parser Command
quit = do
  char 'q'
  opt (string "uit")
  spaces
  eof
  pure Quit

export
command : Parser (Either String Command)
command = map Right help
      <|> map Right env
      <|> aq
      <|> eq
      <|> reduce
      <|> lookup
      <|> save
      <|> delete
      <|> limit
      <|> map Right quit
