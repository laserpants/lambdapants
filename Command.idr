module Command

import Environment
import Term

public export data Command =
  ||| `:help` `:h`         -- Show help
  Help |
  ||| `:env`               -- List environment or show a specific term
  Env (Maybe String) |
  ||| `:aq`                -- Test two terms for alpha equality
  AlphaCompare Term Term |
  ||| `:eq`                -- descr.
  EvalCompare Term Term |
  ||| `:reduce` `:r`       -- descr.
  Reduce Term |
  ||| `:lookup` `:l`       -- Look up a term in the environment
  Lookup Term |
  ||| `:save` `:s`         -- Add a term to the environment
  Save String Term |
  ||| `:delete` `:d`       -- Remove a term from the environment
  Delete String |
  ||| `:quit` `:q`         -- Exit
  Limit Nat |
  ||| `:limit`             -- Set maximum number of reductions
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
         putStrLn (toLower (show (alphaEq a b)))
         pure env
       EvalCompare a b => do
         putStrLn "Evaluate and compare"
         pure env
       Reduce t => do
         putStrLn "Reduce a term"
         pure env
       Lookup t => do
         putStrLn "Look up a term"
         pure env
       Save s t => do
         putStrLn ("Saving term '" ++ s ++ "' to environment")
         pure ((s, t) :: env)
       Delete s => do
         pure env
       Limit max => do
         pure env
       Quit => pure env
