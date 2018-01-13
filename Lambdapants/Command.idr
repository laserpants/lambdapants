module Lambdapants.Command

import Effect.Readline
import Effect.State
import Effect.StdIO
import Effects
import Lambdapants.Term
import Lambdapants.Term.Nats

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

mapE_ : (a -> EffM m b xs (\underscore => xs)) 
     -> List a 
     -> EffM m () xs (\underscore => xs)
mapE_ f xs = mapE f xs *> pure ()

highlight : String -> String
highlight s = "\ESC[0;92m" ++ s ++ "\ESC[0m"

deleteTerm : String -> Eff () [STATE Repl, STDIO, READLINE]
deleteTerm symbol = do
  let xs = dict !get
  if isJust (lookup symbol xs)
     then do update (set_dict (filter ((/= symbol) . fst) xs))
             putStrLn (highlight "Deleted!")
     else putStrLn "There is no term with that name."

saveTerm : String -> Term -> Eff () [STATE Repl, STDIO, READLINE]
saveTerm symbol term =
  case lookup symbol (dict !get) of
       Just found =>
         if found `alphaEq` term
            then putStrLn "This term already exists."
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

printEnv : List (String, Term) -> Eff () [STDIO]
printEnv xs = mapE_ (\s => entry s) xs where
  spaces : Nat -> String
  spaces n = pack (replicate n ' ')
  colWidth : Nat
  colWidth = 2 + foldr (max . Prelude.Strings.length . fst) 0 xs
  entry : (String, Term) -> Eff () [STDIO]
  entry (symb, term) = do
    putStr symb
    putStr (spaces (colWidth `minus` length symb))
    putStrLn (pretty term)

describe : Eff () [STDIO, STATE Repl]
describe = printEnv (dict !get)

termLookup : Term -> Eff () [STDIO, STATE Repl]
termLookup term =
  case map fst (filter (alphaEq term . snd) (dict !get)) of
         [] => case decoded term of
                    Just nat => putStrLn (show nat)
                    Nothing  => putStrLn "This term is not defined."
         xs => mapE_ (\s => putStrLn s) xs

export
execute : Command -> Eff () [STATE Repl, STDIO, READLINE]
execute Help          = putStrLn "Show help"
execute Env           = describe
execute (AlphaEq a b) = putStrLn (toLower (show (alphaEq a b)))
execute (Eq a b)      = putStrLn "Evaluate and compare"
execute (Reduce t)    = putStrLn "Reduce a term"
execute (Lookup t)    = termLookup t
execute (Save s t)    = saveTerm s t *> addDictEntry s
execute (Delete term) = deleteTerm term
execute (Limit max)   = updateLimit max
execute Quit          = pure ()
execute (Eval arg)    = do
  setEvalOrder arg
  let strategy = eval !get
  putStrLn ("Evaluation is in " ++ toLower (show strategy) ++ " order.")
