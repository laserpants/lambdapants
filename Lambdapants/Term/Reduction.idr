module Lambdapants.Term.Reduction

import Lambdapants.Term

total
whnf : Term -> Bool
whnf (Var _)           = True
whnf (Lam _ _)         = True
whnf (App (Lam _ _) _) = False
whnf (App e1 _)        = whnf e1 

total
cbn : Term -> Bool
cbn (Var x)   = Var x
cbn (Lam x e) = Lam x e
cbn (App e1 e2) = 
  if whnf e1 
     then ?a
     else ?b
