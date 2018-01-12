module Lambdapants.Command

import Effect.Readline
import Effect.State
import Effect.StdIO
import Effects
import Lambdapants.Term

public export
Environment : Type
Environment = List (String, Term)

public export
data Strategy = Normal | Applicative

Eq Strategy where
  Normal      == Normal      = True
  Applicative == Applicative = True
  _           == _           = False

Show Strategy where
  show Normal      = "Normal"
  show Applicative = "Applicative"

public export
data Command =
  ||| `:help` `:h` `:?`    -- Show help
  Help |
  ||| `:env`               -- List environment or show a specific term
  Env (Maybe String) |
  ||| `:aq`                -- Test two terms for alpha equality
  AlphaEq Term Term |
  ||| `:eq`                -- descr.
  Eq Term Term |
  ||| `:reduce` `:r`       -- Apply one beta reduction step to the expression
  |||                         to derive a new term
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
  Eval (Maybe Strategy) |
  ||| `:eval`              -- Set evaluation strategy
  Quit

export
Eq Command where
  (Env a)       == (Env b)       = a == b
  (AlphaEq s t) == (AlphaEq u v) = s == u && t == v
  (Eq s t)      == (Eq u v)      = s == u && t == v
  (Reduce s)    == (Reduce t)    = s == t
  (Lookup s)    == (Lookup t)    = s == t
  (Save s t)    == (Save u v)    = s == u && t == v
  (Delete s)    == (Delete t)    = s == t
  (Limit m)     == (Limit n)     = m == n
  (Eval s)      == (Eval t)      = s == t
  Help          == Help          = True
  Quit          == Quit          = True
  _             == _             = False

public export
record Repl where
  constructor ReplState
  dict  : Environment
  limit : Nat
  eval  : Strategy

--Lens : Type -> Type -> Type -> Type -> Type
--Lens s t a b = (f : Type -> Type) -> Functor f -> (a -> f b) -> s -> f t
--first : Lens (a, c) (b, c) a b
--first F inst f (a, b) = map (x => (x, b)) (f a)

saveTerm : String -> Term -> Eff () [STATE Repl]
saveTerm symbol term = update (\st => set_dict ((symbol, term) :: dict st) st)

updateLimit : Nat -> Eff () [STATE Repl]
updateLimit lim = update (set_limit lim)

setEvalOrder : Maybe Strategy -> Eff () [STATE Repl]
setEvalOrder Nothing      = pure ()
setEvalOrder (Just strat) = update (set_eval strat)

export
execute : Command -> Eff () [STATE Repl, STDIO, READLINE]
execute Help = putStrLn "Show help"
execute (Env _) = putStrLn "Show env"
execute (AlphaEq a b) = putStrLn (toLower (show (alphaEq a b)))
execute (Eq a b) = putStrLn "Evaluate and compare"
execute (Reduce t) = putStrLn "Reduce a term"
execute (Lookup t) = putStrLn "Look up a term"
execute (Save s t) = do
  putStr "Saving term "
  putStr ("\ESC[0;93m")
  putStr s
  putStr "\ESC[0m"
  putStrLn " to environment."
  saveTerm s t
  addHistory s
execute (Delete s) = pure ()
execute (Limit max) = updateLimit max
execute (Eval arg) = do
  setEvalOrder arg
  let strategy = eval !get
  putStrLn ("Evaluation is in " ++ toLower (show strategy) ++ " order.")
execute Quit = pure ()
