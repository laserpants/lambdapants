module Lambdapants.Term.Reduction

import Lambdapants.Term

%default total

public export
data Strategy = Normal | Applicative

export
Eq Strategy where
  Normal      == Normal      = True
  Applicative == Applicative = True
  _           == _           = False

export
Show Strategy where
  show Normal      = "Normal"
  show Applicative = "Applicative"

rename : String -> String -> Term -> Term
rename fr to = translate where
  translate (Var v)     = Var (if v == fr then to else v)
  translate (App e1 e2) = App (translate e1) (translate e2)
  translate (Lam x e)   = Lam (if x == fr then to else x) (translate e)

nextSuffix : List String -> Nat
nextSuffix xs = case last' (sort (map trailingDigits xs)) of
                     Nothing => 0
                     Just x => succ (cast x)
where
  trailingDigits : String -> String
  trailingDigits = reverse . pack . takeWhile isDigit . unpack . reverse

export
substitute : String -> Term -> Term -> Term
substitute var expr = translate . sanitized where
  vars : List String
  vars = freeVars expr
  suffix : String
  suffix = "_" ++ show (nextSuffix vars)
  sanitized : Term -> Term
  sanitized (Var v)     = Var v
  sanitized (App e1 e2) = App (sanitized e1) (sanitized e2)
  sanitized (Lam x e)   = if elem x vars
                             then let x' = x ++ suffix in
                                      Lam x' (rename x x' (sanitized e))
                             else Lam x (sanitized e)
  translate : Term -> Term
  translate (Var v)     = if var == v then expr else Var v
  translate (App e1 e2) = App (translate e1) (translate e2)
  translate (Lam x e)   = Lam x (if x == var then e else translate e)

export
normal : Term -> Bool
normal (App (Lam _ _) _) = False
normal (App e1 e2)       = normal e1 && normal e2
normal (Lam _ e)         = normal e
normal _                 = True

export
whnf : Term -> Bool
whnf (App e1 _) = case e1 of
                       Lam _ _   => False
                       otherwise => whnf e1
whnf _          = True

export
cbn : Term -> Term
cbn (Var x)            = Var x
cbn (Lam x e)          = Lam x e
cbn (App (Lam x e) e2) = substitute x e2 e
cbn (App e1 e2)        = App (cbn e1) e2

export
nor : Term -> Term
nor (Var x)            = Var x
nor (Lam x e)          = Lam x (nor e)
nor (App (Lam x e) e2) = substitute x e2 e
nor (App e1 e2)        = if normal e1 then App e1 (nor e2) else App (nor e1) e2

export
aor : Term -> Term
aor (Var x)   = Var x
aor (Lam x e) = Lam x (aor e)
aor (App e1 e2) with (normal e2)
  | False = App e1 (aor e2)
  | True  = case e1 of
                 Lam x e   => substitute x e2 e
                 otherwise => App (aor e1) e2

export
evaluate : Strategy -> Term -> Term
evaluate Normal      = nor
evaluate Applicative = aor
