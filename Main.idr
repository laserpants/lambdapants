module Main

import Lightyear.Strings
import Readline
import Term
import Term.Parser

fancyPutStr : String -> String -> IO ()
fancyPutStr code str = do
  putStr ("\ESC[" ++ code ++ "m")
  putStr str
  putStr "\ESC[0m"

export mkChurch : Nat -> Term
mkChurch n = Lam "f" (Lam "x" nat) where
  nat : Term
  nat = foldr apply (Var "x") (take n (repeat (Term.App (Var "f"))))

Environment : Type
Environment = List (String, Term)

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

  , ("0"       , "\\f.\\x.x")
  , ("1"       , "\\f.\\x.f x")
  , ("2"       , "\\f.\\x.f (f x)")
  , ("3"       , "\\f.\\x.f (f (f x))")
  ])

where
  f : (String, String) -> Maybe (String, Term)
  f (s, e) = case parse term e of
                  Left  _ => Nothing
                  Right t => Just (s, t)

run : Nat -> Term -> IO ()
run count term = do
  when (count > 0) (fancyPutStr "0;32" "\x21d2 ") -- Right arrow
  putStrLn (pretty term)
  when (isRedex term) continue
where
  continue : IO ()
  continue =
    if (count >= 150)
       then do
         fancyPutStr "1;91" "Terminated! "
         putStrLn "Too many reductions."
       else run (succ count) (reduct term)

runWithEnv : Term -> Environment -> IO ()
runWithEnv term env = run 0 (foldr (uncurry substitute) term env)

partial parseUnsafe : String -> Term
parseUnsafe input =
  case parse term input of
       Right term => term

main : IO ()
main = do
  fancyPutStr "1;37" "lambdapants"
  putStrLn " \x03bb_\x03bb version 0.0.1"
  loop
where
  loop : IO ()
  loop = do
    line <- readline "\001\ESC[0;92m\002\x03bb\001\ESC[0m\002 " -- Lambda sign
    case line of
         Just ""  => loop
         Just str => do
           addHistory str
           case parse term str of
                Right t => runWithEnv t stdEnv
                Left  _ => do
                  fancyPutStr "1;91" "Error! "
                  putStrLn "Not a valid term."
           loop
         Nothing => fancyPutStr "0;37" "\nBye!\n"
