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

run : Term -> IO ()
run term = do
  putStrLn (pretty term)
  if isRedex term
     then run (reduct term)
     else pure ()

main : IO ()
main = loop where
  loop : IO ()
  loop = do
    str <- readline "\x03BB "
    case parse term str of
         Left  _    => putStrLn "No parse"
         Right term => run term
    loop
