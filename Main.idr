module Main

import Lightyear.Strings
import Readline
import Term
import Term.Parser

Environment : Type
Environment = List (String, Term)

--mkNat : Nat -> Term
--mkNat 0 = Lam "f" ()

decorate : String -> String -> String
decorate code str = "\x01b[" ++ code ++ "m" ++ str ++ "\x01b[0m"

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
  , ("Y"       , "\\g.(\\x.g (x x)) (\\x.g (x x))") 
-- PAIR := λx.λy.λf.f x y
-- FIRST := λp.p TRUE
-- SECOND := λp.p FALSE
-- NIL := λx.TRUE
-- NULL := λp.p (λx.λy.FALSE)
  , ("0"       , "\\f.\\x.x")
  , ("1"       , "\\f.\\x.f x")
  , ("2"       , "\\f.\\x.f (f x)")
  , ("3"       , "\\f.\\x.f (f (f x))") 
  ])
-- I := λx.x
-- K := λx.λy.x
-- S := λx.λy.λz.x z (y z)
-- B := λx.λy.λz.x (y z)
-- C := λx.λy.λz.x z y
-- W := λx.λy.x y y
-- U := λx.λy.y (x x y)
-- ω := λx.x x
-- Ω := ω ω
-- Y := λg.(λx.g (x x)) (λx.g (x x))
where
  f : (String, String) -> Maybe (String, Term)
  f (s, e) = case parse term e of 
                  Left  _ => Nothing 
                  Right t => Just (s, t)

run : Nat -> Term -> IO ()
run count term = do
  when (count > 0) (putStr ((decorate "0;97" "\x21d2") ++ " ")) -- Right arrow
  putStrLn (pretty term)
  when (isRedex term) continue 
where
  continue : IO ()
  continue = 
    if (count >= 150) 
       then putStrLn ((decorate "1;91" "Terminated! ") ++ "Too many reductions.")
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
    line <- readline ((decorate "0;97" "\x03bb") ++ " ") -- Lambda sign
    case line of
         Just ""  => loop
         Just str => do 
           addHistory str
           case parse term str of 
                Right t => runWithEnv t stdEnv 
                Left  _ => putStrLn ((decorate "1;91" "Error! ") ++ "Not a valid term.")
           loop
         Nothing => putStrLn "Bye!" 
