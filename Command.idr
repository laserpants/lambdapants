module Command

import Environment
import Term

public export data Command =
  Help |
  Env (Maybe String) |
  AlphaCompare Term Term |
  Reduce Term |
  Lookup Term |
  Save String Term |
  Quit

export
execute : Command -> Environment -> IO Environment
execute command env =
  case command of
       Help => do
         putStrLn "Show help"
         pure env
       Env _ => do
         putStrLn "Show env"
         pure env
       AlphaCompare a b => do
         putStrLn (show (alphaEq a b))
         pure env
       Save s t => do
         putStrLn ("Saving term '" ++ s ++ "' to environment")
         pure ((s, t) :: env)
       Quit => pure env
