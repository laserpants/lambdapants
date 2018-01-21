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
  ||| `:eq`                -- TODO: descr.
  Eq Term Term |
  ||| `:reduce` `:r`       -- Apply one beta reduction step to the expression
  |||                         to derive a new term
  Reduce Term |
  -- TODO: Step |                   -- Repeated reduction
  ||| `:whatis` `:w`       -- Look up a term (up to alpha equivalence) in the
  |||                         environment)
  Whatis Term |
  ||| `:set` `:s`          -- Add a term to the environment
  Set String Term |
  ||| `:unset` `:u`        -- Remove a term from the environment
  Unset String |
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
  (Whatis s)    == (Whatis t)    = s == t
  (Set s t)     == (Set u v)     = s == u && t == v
  (Unset s)     == (Unset t)     = s == t
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
  putStrLn ("Evaluation is set to " ++ toLower (show (eval !get)) ++ " order.")
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

termWhatis : Term -> Eff () [STDIO, STATE Repl]
termWhatis term =
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

printHelp : Eff () [STDIO]
printHelp = putStrLn "\
  \Command     Arguments            Description                                                                           \n\
  \-----------------------------------------------------------------------------------------------------------------------\n\
  \<expr>                           Evaluate an expression of the form <term> := <var> | \\<var>.<term> | (<term> <term>).\n\
  \:!                               Run a shell command.                                                                  \n\
  \:h :? :help                      Show this help.                                                                       \n\
  \:env                             List environment.                                                                     \n\
  \:aq         <term> <term>        Test two terms for alpha equality.                                                    \n\
  \:eq         <term> <term>        Compare the normal forms (if reduction terminates) of two terms.                      \n\
  \:r :reduce  <term>               Apply one beta reduction step to the expression.                                      \n\
  \:w :whatis  <term>               Look up a term (up to alpha equivalence) in the environment.                          \n\
  \:s :set     <symbol> <term>      Add a term to the environment.                                                        \n\
  \:u :unset   <symbol>             Remove a term from the environment.                                                   \n\
  \:limit      <number>             Set maximum number of reductions.                                                     \n\
  \:eval       [normal|applicative] Set or show evaluation strategy.                                                      \n\
  \:q :quit                         Exit"

export
execute : Command -> Eff () [STATE Repl, STDIO, SYSTEM, BASELINE]
execute Help          = printHelp
execute Env           = printEnv (dict !get)
execute (AlphaEq a b) = putStrLn (toLower (show (alphaEq a b)))
execute (Eq a b)      = evalAndCompare a b
execute (Reduce t)    = reduce t
execute (Whatis t)    = termWhatis t
execute (Set s t)     = saveTerm s t *> addDictEntry s
execute (Unset term)  = deleteTerm term
execute (Limit max)   = updateLimit max
execute (Shell cmd)   = system cmd *> pure ()
execute (Eval arg)    = setEvalOrder arg
execute _ = pure ()
