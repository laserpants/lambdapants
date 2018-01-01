module Main

import Lightyear.Strings
import Readline
import Term
import Term.Parser

decorate : String -> String -> String
decorate code str = "\ESC[" ++ code ++ "m" ++ str ++ "\ESC[0m"

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

  , ("Y"       , "\\g.(\\x.g (x x)) (\\x.g (x x))") 
  , ("I"       , "\\x.x")
  , ("K"       , "\\x.\\y.x")
  , ("S"       , "\\x.\\y.\\z.x z (y z)")
  , ("B"       , "\\x.\\y.\\z.x (y z)")
  , ("C"       , "\\x.\\y.\\z.x z y")
  , ("W"       , "\\x.\\y.x y y")
  , ("U"       , "\\x.\\y.y (x x y)")
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
  when (count > 0) (putStr (decorate "0;97" "\x21d2" ++ " ")) -- Right arrow
  putStrLn (pretty term)
  when (isRedex term) continue 
where
  continue : IO ()
  continue = 
    if (count >= 150) 
       then putStrLn (decorate "1;91" "Terminated! " ++ "Too many reductions.")
       else run (succ count) (reduct term)

runWithEnv : Term -> Environment -> IO ()
runWithEnv term env = run 0 (foldr (uncurry substitute) term env) 

partial parseUnsafe : String -> Term
parseUnsafe input =
  case parse term input of
       Right term => term

main : IO ()
main = loop where
  loop : IO ()
  loop = do
    line <- readline (decorate "0;92" "\x03bb" ++ " ") -- Lambda sign
    case line of
         Just ""  => loop
         Just str => do 
           addHistory str
           case parse term str of 
                Right t => runWithEnv t stdEnv 
                Left  _ => putStrLn (decorate "1;91" "Error! " ++ "Not a valid term.")
           loop
         Nothing => putStrLn ("\n" ++ decorate "0;37" "Bye!")
