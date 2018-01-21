module Lambdapants.Environment

import Lambdapants.Term
import Lambdapants.Term.Nats
import Lambdapants.Term.Reduction

%default total

public export
Environment : Type
Environment = List (String, Term)

export
substEnv : Term -> Environment -> Term
substEnv = foldr (uncurry substitute)

export
encodeNats : Term -> Term
encodeNats = enc [] where
  enc : List String -> Term -> Term
  enc bound (Var name) =
    if not (elem name bound) && all isDigit (unpack name)
       then let num = cast name in
                if num > 800 -- Treat numbers larger than 800 as literals
                   then Var name
                   else encoded num
       else Var name
  enc bound (App t u) = App (enc bound t) (enc bound u)
  enc bound (Lam v t) = Lam v (enc (v :: bound) t)

export
closed : Term -> Environment -> Term
closed = substEnv . encodeNats
