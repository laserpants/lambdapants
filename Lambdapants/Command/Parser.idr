module Lambdapants.Command.Parser

import Lambdapants.Command
import Lambdapants.Term
import Lambdapants.Term.Parser
import Lambdapants.Term.Reduction
import Lightyear
import Lightyear.Char
import Lightyear.Strings

symbolArg : Parser String
symbolArg = symbol

termArg : Parser Term
termArg = parens appl <|>| map Var symbol

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

arity : ArgT -> Nat
arity (Arg0 _      ) = 0
arity (Arg1 _ _    ) = 1
arity (Arg2 _ _ _  ) = 2
arity (Arg3 _ _ _ _) = 3

commands : List (String, (ArgT, String))
commands =
  [ ("q"       , (Arg0 Quit                      , ""))
  , ("quit"    , (Arg0 Quit                      , ""))
  , ("limit"   , (Arg1 Limit natArg              , "<number>"))
  , ("u"       , (Arg1 Unset symbolArg           , "<symbol>"))
  , ("unset"   , (Arg1 Unset symbolArg           , "<symbol>"))
  , ("s"       , (Arg2 Set symbolArg termArg     , "<symbol> <term>"))
  , ("set"     , (Arg2 Set symbolArg termArg     , "<symbol> <term>"))
  , ("w"       , (Arg1 Whatis termArg            , "<term>"))
  , ("whatis"  , (Arg1 Whatis termArg            , "<term>"))
  , ("r"       , (Arg1 Reduce termArg            , "<term>"))
  , ("reduce"  , (Arg1 Reduce termArg            , "<term>"))
  , ("eq"      , (Arg2 Eq termArg termArg        , "<term> <term>"))
  , ("aq"      , (Arg2 AlphaEq termArg termArg   , "<term> <term>"))
  , ("env"     , (Arg0 Env                       , ""))
  , ("eval"    , (Arg1 Eval (opt stratArg)       , "[normal | applicative]"))
  , ("?"       , (Arg0 Help                      , ""))
  , ("h"       , (Arg0 Help                      , ""))
  , ("help"    , (Arg0 Help                      , "")) ]

args : ArgT -> Parser Command
args (Arg0 constr) = do
  spaces
  eof
  pure constr
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
  maybe (Left err) (uncurry compile) (lookup command commands)
where
  compile : ArgT -> String -> Either String Command
  compile argt hint = do
    case parse (args argt) argstr of
         Left _  => Left (if 0 == arity argt
                         then command ++ " is like Chuck Norris. \
                           \It takes no arguments."
                         else "Usage is :" ++ command ++ " " ++ hint)
         right => right
  err : String
  err = "\ESC[0;91mUnrecognized command: " ++ command ++ "\ESC[0m"

export
parseCmd : String -> Either String Command
parseCmd str = do
  case words str of
       (w :: ws) =>
            let args = unwords ws in
                if "!" == w
                   then Right (Shell args)
                   else parseArgs w args
       otherwise => Left "error"
