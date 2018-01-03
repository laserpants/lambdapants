module Environment

import Term

public export Environment : Type
Environment = List (String, Term)
