module Main

import Lightyear.Strings
import Term
import Term.Parser

term1_0 : Term
term1_0 = App (Lam "f" (Lam "x" (App (Var "f") (Var "x")))) (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))

main : IO ()
main = do
  putStrLn (pretty (reduct term1_0))
  putStrLn (pretty (reduct (reduct term1_0)))
  putStrLn (pretty (reduct (reduct term1_0)))
