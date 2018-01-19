module Lambdapants.Command

import Effect.Baseline
import Effect.State
import Effect.StdIO
import Effect.System
import Effects
import Lambdapants.Environment
import Lambdapants.Term
import Lambdapants.Term.Nats
import Lambdapants.Term.Reduction

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
  ||| '!'                  -- Run shell command
  Shell String |
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
  (Shell a)     == (Shell b)     = a == b
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

deleteTerm : String -> Eff () [STATE Repl, STDIO, BASELINE]
deleteTerm symbol = do
  let xs = dict !get
  if isJust (lookup symbol xs)
     then do update (set_dict (filter ((/= symbol) . fst) xs))
             putStrLn (highlight "Deleted!")
     else putStrLn "There is no term with that name."

updateEnv : String -> Term -> Eff () [STATE Repl, STDIO]
updateEnv symbol term = do
  update (set_dict ((symbol, term) :: dict !get))
  putStrLn (highlight "Saved!")

saveTerm : String -> Term -> Eff () [STATE Repl, STDIO, BASELINE]
saveTerm symbol term = save (closed term (dict !get)) where
  save : Term -> Eff () [STATE Repl, STDIO, BASELINE]
  save closed_term =
    case lookup symbol (dict !get) of
         Nothing => updateEnv symbol closed_term
         Just this =>
           if this `alphaEq` closed_term
              then putStrLn "This term already exists."
              else do
                what <- baseline "Replace existing entry (y[es] to confirm)? "
                when (Just "y" == what || Just "yes" == what) 
                  (updateEnv symbol closed_term)

updateLimit : Nat -> Eff () [STATE Repl]
updateLimit lim = update (set_limit lim)

setEvalOrder : Maybe Strategy -> Eff () [STDIO, STATE Repl]
setEvalOrder strategy = do
  set strategy
  putStrLn ("Evaluation proceeds in " ++ toLower (show (eval !get)) ++ " order.")
where
  set : Maybe Strategy -> Eff () [STATE Repl]
  set Nothing  = pure ()
  set (Just s) = update (set_eval s)

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

termLookup : Term -> Eff () [STDIO, STATE Repl]
termLookup term =
  case map fst (filter (alphaEq term . snd) (dict !get)) of
         [] => case decoded term of
                    Just nat => putStrLn (show nat)
                    Nothing  => putStrLn "This term is not defined."
         xs => mapE_ (\s => putStrLn s) xs

reduce : Term -> Eff () [STDIO, STATE Repl]
reduce term = do
  let term' = closed term (dict !get)
  putStrLn (pretty term')
  putStrLn ("\x21d2 " ++ pretty (evaluate (eval !get) term'))

evalAndCompare : Term -> Term -> Eff () [STDIO, STATE Repl]
evalAndCompare s t = do
  putStrLn "TODO"
  pure ()

export
execute : Command -> Eff () [STATE Repl, STDIO, SYSTEM, BASELINE]
execute Help          = putStrLn "Show help"
execute Env           = printEnv (dict !get)
execute (AlphaEq a b) = putStrLn (toLower (show (alphaEq a b)))
execute (Eq a b)      = evalAndCompare a b
execute (Reduce t)    = reduce t
execute (Lookup t)    = termLookup t
execute (Save s t)    = saveTerm s t *> addDictEntry s
execute (Delete term) = deleteTerm term
execute (Limit max)   = updateLimit max
execute (Shell cmd)   = system cmd *> pure ()
execute (Eval arg)    = setEvalOrder arg
execute _ = pure ()
