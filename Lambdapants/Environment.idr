module Lambdapants.Environment

import Lambdapants.Term

public export
Environment : Type
Environment = List (String, Term)
