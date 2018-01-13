module Lambdapants.Term

||| In the lambda calculus, a term is one of three things:
||| * A variable is a term;
||| * Application of two terms is a term; and
||| * A lambda abstraction is a term.
|||
||| Nothing else is a term. Application is left-associative, so the term
||| `(s t u)` is the same as `(s t) u`. One often omits outermost parentheses.
||| In abstractions, the body extends as far to the right as possible.
public export
data Term : Type where
  ||| Variable
  Var : String -> Term
  ||| Lambda abstraction
  Lam : String -> Term -> Term
  ||| Application
  App : Term -> Term -> Term

export
Eq Term where
  (Var a)   == (Var b)   = a == b
  (Lam x t) == (Lam y u) = x == y && t == u
  (App t u) == (App v w) = t == v && u == w
  _         == _         = False

export
Show Term where
  show (Var v)   = "Var "  ++ show v
  show (App t u) = "App (" ++ show t ++ ") ("
                           ++ show u ++ ")"
  show (Lam x t) = "Lam "  ++ show x ++ " ("
                           ++ show t ++ ")"

mutual
  lam : Term -> String
  lam (Lam x t) = "\x03BB" ++ x ++ "." ++ lam t
  lam term = app term

  app : Term -> String
  app (App t u) = app t ++ " " ++ pretty u
  app term = pretty term

  ||| Translate the given term to a pretty-printed string.
  export
  pretty : Term -> String
  pretty term =
    case term of
         Lam _ _ => "(" ++ lam term ++ ")"
         App _ _ => "(" ++ app term ++ ")"
         Var var => var

||| Return a list of all variables which appear free in the term *t*.
export total
freeVars : (t : Term) -> List String
freeVars (Var v)   = [v]
freeVars (Lam v t) = delete v (freeVars t)
freeVars (App t u) = freeVars t `union` freeVars u

||| Return a boolean to indicate whether the variable *v* appears free in the
||| term *t*.
export total
isFreeIn : (v : String) -> (t : Term) -> Bool
isFreeIn var term = elem var (freeVars term)

||| Return all variables (free and bound) which appear in the term *t*.
export total
vars : (t : Term) -> List String
vars (Var v)   = [v]
vars (Lam v t) = v :: vars t
vars (App t u) = vars t `union` vars u

||| De Bruijn-indexed intermediate representation for more convenient alpha
||| equivalence comparison of terms.
data Indexed : Type where
  ||| Bound variable (depth indexed)
  Bound : Nat -> Indexed
  ||| Free variable
  Free  : String -> Indexed
  ||| Application
  IxApp : Indexed -> Indexed -> Indexed
  ||| Lambda abstraction
  IxLam : Indexed -> Indexed

Show Indexed where
  show (Bound n)   = "Bound "  ++ show n
  show (Free v)    = "Free "   ++ show v
  show (IxApp t u) = "IxApp (" ++ show t ++ ") ("
                               ++ show u ++ ")"
  show (IxLam t)   = "IxLam (" ++ show t ++ ")"

Eq Indexed where
  (Bound m)   == (Bound n)   = m == n
  (Free v)    == (Free w)    = v == w
  (IxApp t u) == (IxApp v w) = t == v && u == w
  (IxLam t)   == (IxLam u)   = t == u
  _           == _           = False

||| Translate the term *t* to canonical De Bruijn (depth-indexed) form.
total
toIndexed : (t : Term) -> Indexed
toIndexed = indexed []
where
  indexed : List String -> Term -> Indexed
  indexed bound (Var x)   = maybe (Free x) Bound (elemIndex x bound)
  indexed bound (App t u) = IxApp (indexed bound t) (indexed bound u)
  indexed bound (Lam x t) = IxLam (indexed (x :: bound) t)

||| Return a boolean to indicate whether two terms are alpha equivalent; that
||| is, whether one can be converted to the other purely by renaming of bound
||| variables.
export total
alphaEq : Term -> Term -> Bool
alphaEq t u = toIndexed t == toIndexed u
