module Lambdapants.Command.Parser

import Lambdapants.Command
import Lambdapants.Term
import Lambdapants.Term.Parser
import Lightyear
import Lightyear.Char
import Lightyear.Strings

symbolArg : Parser String
symbolArg = atom

termArg : Parser Term
termArg = (char '(' *> term <* char ')') <|>| map Var atom

natArg : Parser Nat
natArg = (cast . pack) <$> some (satisfy isDigit)

stratArg : Parser Strategy
stratArg = norm <|> appl where
  norm : Parser Strategy
  norm = string "normal" *> pure Normal
  appl : Parser Strategy
  appl = string "applicative" *> pure Applicative

data ArgT
  = Arg0 Command
  | Arg1 (a -> Command)           (Parser a)
  | Arg2 (a -> b -> Command)      (Parser a) (Parser b)
  | Arg3 (a -> b -> c -> Command) (Parser a) (Parser b) (Parser c)

commands : List (String, (ArgT, String))
commands =
  [ ("q"       , (Arg0 Quit                      , ""))
  , ("quit"    , (Arg0 Quit                      , ""))
  , ("limit"   , (Arg1 Limit natArg              , "<number>"))
  , ("d"       , (Arg1 Delete symbolArg          , "<symbol>"))
  , ("delete"  , (Arg1 Delete symbolArg          , "<symbol>"))
  , ("s"       , (Arg2 Save symbolArg termArg    , "<symbol> <term>"))
  , ("save"    , (Arg2 Save symbolArg termArg    , "<symbol> <term>"))
  , ("l"       , (Arg1 Lookup termArg            , "<term>"))
  , ("lookup"  , (Arg1 Lookup termArg            , "<term>"))
  , ("r"       , (Arg1 Reduce termArg            , "<term>"))
  , ("reduce"  , (Arg1 Reduce termArg            , "<term>"))
  , ("eq"      , (Arg2 Eq termArg termArg        , "<term> <term>"))
  , ("aq"      , (Arg2 AlphaEq termArg termArg   , "<term> <term>"))
  , ("env"     , (Arg1 Env (opt symbolArg)       , "[<symbol>]"))
  , ("eval"    , (Arg1 Eval (opt stratArg)       , "[normal | applicative]"))
  , ("?"       , (Arg0 Help                      , ""))
  , ("h"       , (Arg0 Help                      , ""))
  , ("help"    , (Arg0 Help                      , ""))
  ]

args : ArgT -> Parser Command
args (Arg0 constr) = pure constr
args (Arg1 constr arg1) = do
  a <- arg1
  spaces
  eof
  pure (constr a)
args (Arg2 constr arg1 arg2) = do
  a <- arg1
  some space
  b <- arg2
  spaces
  eof
  pure (constr a b)
args (Arg3 constr arg1 arg2 arg3) = do
  a <- arg1
  some space
  b <- arg2
  some space
  c <- arg3
  spaces
  eof
  pure (constr a b c)

parseArgs : String -> String -> Either String Command
parseArgs command argstr =
  maybe (Left unknown) (uncurry compile) (lookup command commands)
where
  compile : ArgT -> String -> Either String Command
  compile argt hint = do
    case parse (args argt) argstr of
         Left _  => Left ("Usage is :" ++ command ++ " " ++ hint)
         right   => right
  unknown : String
  unknown = "\ESC[0;91mUnrecognized command: " ++ command ++ "\ESC[0m"

export
parseCmd : String -> Either String Command
parseCmd str = do
  case words str of
       (w :: ws) => parseArgs w (unwords ws)
       otherwise => Left "error"
