module Main

import Effect.Baseline
import Effect.State
import Effect.StdIO
import Effect.System
import Effects
import Lambdapants.Command
import Lambdapants.Command.Parser
import Lambdapants.Environment
import Lambdapants.Term
import Lambdapants.Term.Nats
import Lambdapants.Term.Parser
import Lambdapants.Term.Reduction
import Lightyear.Strings

ansiPut : String -> String -> Eff () [STDIO]
ansiPut code str = do
  putStr ("\ESC[" ++ code ++ "m")
  putStr str
  putStr "\ESC[0m"

stdEnv : Environment
stdEnv = catMaybes (map f
  [ ("plus"    , "\\m.\\n.\\f.\\x.m f (n f x)")
  , ("succ"    , "\\n.\\f.\\x.f (n f x)")
  , ("pred"    , "\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)")
  , ("mult"    , "\\m.\\n.\\f.m (n f)")
  , ("sub"     , "\\m.\\n.n pred m")
  , ("pow"     , "\\b.\\e.e b")
  , ("id"      , "\\x.x")
  , ("true"    , "\\x.\\y.x")
  , ("false"   , "\\x.\\y.y")
  , ("and"     , "\\p.\\q.p q p")
  , ("or"      , "\\p.\\q.p p q")
  , ("not"     , "\\p.p false true")
  , ("if"      , "\\p.\\a.\\b.p a b")
  , ("is_zero" , "\\n.n (\\x.false) true")
  , ("leq"     , "\\m.\\n.is_zero (sub m n)")
  , ("zero"    , "\\f.\\x.x")
  , ("fact"    , "\\k.k (\\p.p (\\a.\\b.\\g.g (\\f.\\x.f (a f x)) (\\f.a (b f))))(\\g.g (\\h.h) (\\h.h)) (\\a.\\b.b)")

  -- SKI combinators

  , ("I"       , "\\x.x")
  , ("K"       , "\\x.\\y.x")
  , ("S"       , "\\x.\\y.\\z.x z (y z)")
  , ("B"       , "\\x.\\y.\\z.x (y z)")
  , ("C"       , "\\x.\\y.\\z.x z y")
  , ("W"       , "\\x.\\y.x y y")
  , ("U"       , "\\x.\\y.y (x x y)")
  , ("Y"       , "\\g.(\\x.g (x x)) (\\x.g (x x))")
  , ("omega"   , "\\x.x x")
  , ("Omega"   , "omega omega")

  , ("pair"    , "\\x.\\y.\\f.f x y")
  , ("first"   , "\\p.p true")
  , ("second"  , "\\p.p false")
  , ("nil"     , "\\x.true")
  , ("null"    , "\\p.p (\\x.\\y.false)")
  ])

where
  f : (String, String) -> Maybe (String, Term)
  f (s, e) = case parse term e of
                  Left  _ => Nothing
                  Right t => Just (s, t)

partial parseUnsafe : String -> Term
parseUnsafe input =
  case parse term input of
       Right term => term

run_ : Nat -> Term -> Eff () [STATE Repl, STDIO]
run_ count term = do
  when (count > 0) (ansiPut "0;32" " \x21d2 ") -- Right arrow
  putStrLn (pretty term)
  when (not (normal term)) continue
where
  continue : Eff () [STATE Repl, STDIO]
  continue = do
    let maxr = limit !get
    if (count >= maxr)
       then do
         ansiPut "0;91" ("Terminated after " ++ show maxr ++ " reductions. \
           \Use :limit to increase depth.")
         putChar '\n'
       else run_ (succ count) (evaluate (eval !get) term)

runWithEnv : Term -> Eff () [STATE Repl, STDIO]
runWithEnv term = run_ 0 (closed term (dict !get))

exitMsg : Eff () [STDIO]
exitMsg = ansiPut "0;37" "Bye!" *> putChar '\n'

loop : Eff () [STATE Repl, STDIO, SYSTEM, BASELINE]
loop = do
  line <- baseline "\x03bb " -- Lambda sign
  case map trim line of
       Just ""  => loop
       Just ":" => loop
       Just str =>
         if ':' == strHead str
            then do
              case parseCmd (strTail str) of
                   Left err => do
                     putStrLn err
                     loop
                   Right action => do
                     execute action
                     if Quit == action then exitMsg else loop
            else do
              case parse term str of
                   Right t => runWithEnv t
                   otherwise => do
                     ansiPut "0;91" "Not a valid term."
                     putStrLn "\nThe format is \
                       \<term> := <var> | \\<var>.<term> | (<term> <term>)"
              loop
       Nothing => putChar '\n' *> exitMsg

prog : Eff () [STATE Repl, STDIO, SYSTEM, BASELINE]
prog = do
  (ReplState env _ _) <- get
  addDictEntries (map fst env)
  addDictEntry "normal"
  addDictEntry "applicative"
  ansiPut "1;37" "lambdapants"
  putStrLn " \x03bb_\x03bb version 0.0.1"
  putStrLn "Type :h for help"
  readHistory ".history"
  loop
  writeHistory ".history"

main : IO ()
main = Effects.runInit [ReplState stdEnv 150 Normal, (), (), ()] prog
