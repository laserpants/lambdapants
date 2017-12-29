module Main

import Lightyear.Strings
import Readline
import Term
import Term.Parser

term1_0 : Term
term1_0 = App (Lam "f" (Lam "x" (App (Var "f") (Var "x")))) (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))

test : IO ()
test = do
  putStrLn (pretty term1_0)
  putStrLn (pretty (reduct term1_0))
  putStrLn (pretty (reduct (reduct term1_0)))
  putStrLn (pretty (reduct (reduct (reduct term1_0))))

Environment : Type
Environment = List (String, Term)

env : Environment
env = catMaybes (map f
  [ ("plus"    , "\\m.\\n.\\f.\\x.m f (n f x)")
  , ("succ"    , "\\n.\\f.\\x.f (n f x)")
  , ("pred"    , "\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)")
  , ("mult"    , "\\m.\\n.\\f.m (n f)")
  , ("sub"     , "\\m.\\n.n pred m")
  , ("pow"     , "\\b.\\e.e b")
  , ("id"      , "\\x.x")
  , ("true"    , "\\x.\\y.x")
  , ("false"   , "\\x.\\y.y")
-- AND := λp.λq.p q p
-- OR := λp.λq.p p q
-- NOT := λp.p FALSE TRUE
-- IFTHENELSE := λp.λa.λb.p a b
  , ("is_zero" , "\\n.n (\\x.false) true")
--  LEQ := λm.λn.ISZERO (SUB m n)
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
    putStr "? "
    str <- getLine -- readline "\x03BB "
    case parse term str of
         Left  _ => putStrLn "No parse"
         Right t => runWithEnv t env 
    loop
