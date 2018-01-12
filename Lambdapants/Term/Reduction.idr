module Lambdapants.Term.Reduction

import Lambdapants.Term

%default total

--another : String -> String
--another name =
--  case unpack name of
--       (c :: [])      => if 'a' <= c && c < 'z'
--                            then pack [succ c]
--                            else pack (c :: '0' :: [])
--       (b :: c :: []) => if '0' <= c && c < '9'
--                            then pack (b :: succ c :: [])
--                            else name ++ "'"
--       _              => name ++ "'"
--
--fresh : Term -> String -> String
--fresh expr = diff where
--  names : List String
--  names = freeVars expr
--  diff : String -> String
--  diff x = let x' = another x in if x' `elem` names then diff x' else x'

rename : String -> String -> Term -> Term
rename fr to = translate where
  translate (Var v)     = Var (if v == fr then to else v)
  translate (App e1 e2) = App (translate e1) (translate e2)
  translate (Lam x e)   = Lam (if x == fr then to else x) (translate e)

substitute : String -> Term -> Term -> Term
substitute var expr term = translate (sanitized term) where
  vars : List String
  vars = freeVars expr
  sanitized : Term -> Term
  sanitized (Var v)     = Var v
  sanitized (App e1 e2) = App (sanitized e1) (sanitized e2)
  sanitized (Lam x e)   = if elem x vars 
                             then let x' = x ++ "_0" {- TODO -} in 
                                      Lam x' (rename x x' e) 
                             else Lam x e
  translate : Term -> Term
  translate (Var v)     = if var == v then expr else Var v
  translate (App e1 e2) = App (translate e1) (translate e2)
  translate (Lam x e)   = 
    if x == var -- Is the variable we are susbstituting for re-bound?
       then Lam x e 
       else Lam x (translate e)

--       then Lam x e -- If the variable we are susbstituting for is re-bound
--       else do 
--         if x `isFreeIn` expr 
--            then let x' = x -- fresh expr x 
--                     e' = alphaRename x x' e in
--                 Lam x' (subst e')
--            else Lam x  (subst e)

--  subst (Lam x e) with (x == var)
--    | True  = Lam x e -- If the variable we are susbstituting for is re-bound
--    | otherwise = 
--      if x `isFreeIn` expr 
--         then let x' = ?f -- fresh expr x 
--                  e' = alphaRename x x' e in
--              Lam x' (subst e')
--         else Lam x  (subst e)

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
