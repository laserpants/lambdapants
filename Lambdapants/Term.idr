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

--||| Return a boolean to indicate whether the term is reducible.
--export total
--isRedex : Term -> Bool
--isRedex (App (Lam _ _) _) = True
--isRedex (App e1 e2)       = isRedex e1 || isRedex e2
--isRedex (Lam _ e1)        = isRedex e1
--isRedex _                 = False

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
--
--alphaRename : String -> String -> Term -> Term
--alphaRename fr to term =
--  case term of
--       (Var v)     => Var (if v == fr then to else v)
--       (App e1 e2) => App (alphaRename fr to e1) (alphaRename fr to e2)
--       (Lam x e)   => Lam (if x == fr then to else x) (alphaRename fr to e)
--
--||| Perform the substitution `s[ n := e ]`.
--||| @n - a variable to substitute for
--||| @e - the term that the variable *n* will be replaced with
--||| @s - the original term
--export
--substitute : (n : String) -> (e : Term) -> (s : Term) -> Term
--substitute var expr = subst where
--  subst : Term -> Term
--  subst (Var v)     = if var == v then expr else Var v
--  subst (App e1 e2) = App (subst e1) (subst e2)
--  subst (Lam x e) with (x == var)
--    | True  = Lam x e -- If the variable we are susbstituting for is re-bound
--    | False = if x `isFreeIn` expr
--                 then let x' = fresh expr x
--                          e' = alphaRename x x' e in
--                      Lam x' (subst e')
--                 else Lam x  (subst e)

-- https://www.itu.dk/~sestoft/papers/sestoft-lamreduce.pdf

--export
--whnf : Term -> Term
--whnf (Var x)     = Var x
--whnf (Lam x e)   = Lam x e
--whnf (App e1 e2) =
--  case whnf e1 of
--       (Lam x e) => whnf (substitute x e2 e)
--       e1'       => App e1' e2
--
--isWhnf : Term -> Bool
--isWhnf (Var _)     = True
--isWhnf (Lam _ _)   = True
--isWhnf (App e1 e2) = ?x
--
--||| Normal order
--export
--nor : Term -> Term
--nor (Var x)     = Var x
--nor (Lam x e)   = Lam x (nor e)
--nor (App e1 e2) =
--  case whnf e1 of
--       (Lam x e) => substitute x e2 e
--       --e1'       => App (nor e1') (nor e2)
--       e1'       => App e1' e2
--
--appl : Term -> Term
--appl = ?app

-- ||| Apply beta reduction in normal order to the expression *e* to derive a new
-- ||| term. This function is defined in terms of *substitute*.
-- export
-- reduce_norm : (e : Term) -> Term
-- reduce_norm (App (Lam v t) s) = substitute v s t
-- reduce_norm (Lam v t) = Lam v (reduce_norm t)
-- reduce_norm (App t u) = if isRedex t
--                            then App (reduce_norm t) u
--                            else App t (reduce_norm u)
-- reduce_norm term = term
--
-- ||| Apply beta reduction in applicative order (I think).
-- export
-- reduce_appl : (e : Term) -> Term
-- reduce_appl (App (Lam v t) s) = if isRedex s
--                                    then App (Lam v t) (reduce_appl s)
--                                    else substitute v s t
-- reduce_appl (App t u) = if isRedex u
--                            then App t (reduce_appl u)
--                            else App (reduce_appl t) u
-- reduce_appl (Lam v t) = Lam v (reduce_appl t)
-- reduce_appl term = term

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
