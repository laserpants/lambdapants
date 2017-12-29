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
  , ("id"      , "\\x.x")
  , ("true"    , "\\x.\\y.x")
  , ("false"   , "\\x.\\y.y")
  , ("is_zero" , "\\n.n (\\x.false) true")
  , ("0"       , "\\f.\\x.x")
  , ("1"       , "\\f.\\x.f x")
  , ("2"       , "\\f.\\x.f (f x)")
  , ("3"       , "\\f.\\x.f (f (f x))") ])
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
