module Lambdapants.Term.Nats

import Lambdapants.Term

%default total

||| Return the Church encoded term corresponding to the provided number.
export
encoded : Nat -> Term
encoded n = Lam "f" (Lam "x" nat) where
  nat : Term
  nat = foldr apply (Var "x") (replicate n (Term.App (Var "f")))

apps : String -> String -> Term -> Maybe Nat
apps f x = count 1 where
  count : Nat -> Term -> Maybe Nat
  count n (App (Var g) (Var v)) = if g == f && v == x then Just n else Nothing
  count n (App (Var g) e)       = if g == f then count (succ n) e else Nothing
  count _ _                     = Nothing

export
decoded : Term -> Maybe Nat
decoded (Lam f (Lam x term)) = 
  case term of
       Var v   => if v == x then Just 0 else Nothing
       App _ _ => apps f x term
       Lam _ _ => Nothing
decoded _ = Nothing
