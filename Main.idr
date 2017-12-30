module Main

import Lightyear.Strings
import Readline
import Term
import Term.Parser

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
-- PAIR := λx.λy.λf.f x y
-- FIRST := λp.p TRUE
-- SECOND := λp.p FALSE
-- NIL := λx.TRUE
-- NULL := λp.p (λx.λy.FALSE)
  , ("0"       , "\\f.\\x.x")
  , ("1"       , "\\f.\\x.f x")
  , ("2"       , "\\f.\\x.f (f x)")
  , ("3"       , "\\f.\\x.f (f (f x))") 
  , ("Y"       , "\\g.(\\x.g (x x)) (\\x.g (x x))") 
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

run : Term -> IO ()
run term = do
  putStrLn (pretty term)
  when (isRedex term) (run (reduct term))

runWithEnv : Term -> Environment -> IO ()
runWithEnv term env = run (foldr (uncurry substitute) term env) 

main : IO ()
main = loop where
  loop : IO ()
  loop = do
    line <- readline "\x03BB "
    case line of
         Just ""  => loop
         Just str => do 
           case parse term str of 
                Right t => runWithEnv t stdEnv 
                Left  _ => putStrLn "Why you no parse?"
           loop
         Nothing => putStrLn "Bye!" 
