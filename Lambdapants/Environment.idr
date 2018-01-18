module Lambdapants.Environment

import Lambdapants.Term
import Lambdapants.Term.Reduction

public export
Environment : Type
Environment = List (String, Term)

export
substEnv : Term -> Environment -> Term
substEnv = foldr (uncurry substitute) 
