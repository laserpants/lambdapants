module Term

||| In the lambda calculus, a term is one of three things:
||| * A variable is a term; 
||| * Application of two terms is a term; and
||| * A lambda abstraction is a term. 
|||
||| Nothing else is a term. Application is left-associative, so the term 
||| `(s t u)` is the same as `(s t) u`. One often omits outermost parentheses. 
||| In abstractions, the body extends as far to the right as possible.
public export data Term : Type -> Type where
  ||| Variable
  Var : (v : t) -> Term t
  ||| Lambda abstraction
  Lam : (v : t) -> Term t -> Term t
  ||| Application
  App : Term t  -> Term t -> Term t

export Eq t => Eq (Term t) where
  (Var a)   == (Var b)   = a == b
  (Lam x t) == (Lam y u) = x == y && t == u
  (App t u) == (App v w) = t == v && u == w
  _         == _         = False

export Show t => Show (Term t) where
  show (Var v)   = "Var "  ++ show v
  show (App t u) = "App (" ++ show t ++ ") (" 
                           ++ show u ++ ")"
  show (Lam x t) = "Lam "  ++ show x ++ "(" 
                           ++ show t ++ ")"

||| Return a list of all variables which appear free in the term 't'.
export total freeVars : { t : Type } -> Eq t => Term t -> List t
freeVars (Var v)   = [v]
freeVars (Lam v t) = delete v (freeVars t)
freeVars (App s t) = freeVars s `union` freeVars t

export total isRedex : { t : Type } -> Term t -> Bool
isRedex (App (Lam _ _) _) = True
isRedex (App e1 e2)       = isRedex e1 || isRedex e2
isRedex (Lam _ e1)        = isRedex e1
isRedex _                 = False
