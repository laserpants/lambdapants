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
  ||| `:env`               -- List environment
  Env |
  ||| `:aq`                -- Test two terms for alpha equality
  AlphaEq Term Term |
  ||| `:eq`                -- descr.
  Eq Term Term |
  ||| `:reduce` `:r`       -- Apply one beta reduction step to the expression
  |||                         to derive a new term
  Reduce Term |
  ||| `:lookup` `:l`       -- Look up a term in the environment (up to alpha
  |||                         equivalence)
  Lookup Term |
  ||| `:save` `:s`         -- Add a term to the environment
  Save String Term |
  ||| `:delete` `:d`       -- Remove a term from the environment
  Delete String |
  ||| `:limit`             -- Set maximum number of reductions
  Limit Nat |
  ||| `:eval`              -- Set/show evaluation strategy
  Eval (Maybe Strategy) |
  ||| `:quit` `:q`         -- Exit
  Quit

export
Eq Command where
  (AlphaEq s t) == (AlphaEq u v) = s == u && t == v
  (Eq s t)      == (Eq u v)      = s == u && t == v
  (Reduce s)    == (Reduce t)    = s == t
  (Lookup s)    == (Lookup t)    = s == t
  (Save s t)    == (Save u v)    = s == u && t == v
  (Delete s)    == (Delete t)    = s == t
  (Limit m)     == (Limit n)     = m == n
  (Eval s)      == (Eval t)      = s == t
  Env           == Env           = True
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

highlight : String -> String
highlight s = "\ESC[0;92m" ++ s ++ "\ESC[0m"

deleteTerm : String -> Eff () [STATE Repl, STDIO, READLINE]
deleteTerm symbol = do
  let xs = dict !get
  case lookup symbol xs of
       Just found => do
         update (set_dict (filter ((/= symbol) . fst) xs))
         putStrLn (highlight "Deleted!")
       Nothing => putStrLn "There is no term with that name."

saveTerm : String -> Term -> Eff () [STATE Repl, STDIO, READLINE]
saveTerm symbol term = do
  case lookup symbol (dict !get) of
       Just found => do
         if found `alphaEq` term
            then do
              putStr "The term '"
              putStr (highlight symbol)
              putStrLn "' is already present."
            else do
              answer <- readline "Replace existing entry (y[es] to confirm)? "
              when (Just "y" == answer || Just "yes" == answer) save
       Nothing => save
where
  save : Eff () [STATE Repl, STDIO, READLINE]
  save = do
    update (set_dict ((symbol, term) :: dict !get))
    putStrLn (highlight "Saved!")

updateLimit : Nat -> Eff () [STATE Repl]
updateLimit lim = update (set_limit lim)

setEvalOrder : Maybe Strategy -> Eff () [STATE Repl]
setEvalOrder Nothing      = pure ()
setEvalOrder (Just strat) = update (set_eval strat)

describe : Eff () [STATE Repl, STDIO]
describe = mapE (\x => do
                putStr (fst x)
                putStr " "
                putStrLn (pretty (snd x)))
                (dict !get) *> pure ()
--describe (Just term) =
--  case lookup term (dict !get) of
--       Nothing => putStrLn "Sorry, there is no term with that name."
--       Just it => putStrLn (pretty it)

export
execute : Command -> Eff () [STATE Repl, STDIO, READLINE]
execute Help          = putStrLn "Show help"
execute Env           = describe
execute (AlphaEq a b) = putStrLn (toLower (show (alphaEq a b)))
execute (Eq a b)      = putStrLn "Evaluate and compare"
execute (Reduce t)    = putStrLn "Reduce a term"
execute (Lookup t)    = putStrLn "Look up a term"
execute (Save s t)    = saveTerm s t *> addDictEntry s
execute (Delete term) = deleteTerm term
execute (Limit max)   = updateLimit max
execute Quit          = pure ()
execute (Eval arg)    = do
  setEvalOrder arg
  let strategy = eval !get
  putStrLn ("Evaluation is in " ++ toLower (show strategy) ++ " order.")
