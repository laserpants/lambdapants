module Term

||| In the lambda calculus, a term is one of three things:
||| * A variable is a term;
||| * Application of two terms is a term; and
||| * A lambda abstraction is a term.
|||
||| Nothing else is a term. Application is left-associative, so the term
||| `(s t u)` is the same as `(s t) u`. One often omits outermost parentheses.
||| In abstractions, the body extends as far to the right as possible.
public export data Term : Type where
  ||| Variable
  Var : String -> Term
  ||| Lambda abstraction
  Lam : String -> Term -> Term
  ||| Application
  App : Term -> Term -> Term

export Eq Term where
  (Var a)   == (Var b)   = a == b
  (Lam x t) == (Lam y u) = x == y && t == u
  (App t u) == (App v w) = t == v && u == w
  _         == _         = False

export Show Term where
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

  ||| Translate the given term to a Pretty-printed string.
  export pretty : Term -> String
  pretty term =
    case term of
         Lam _ _ => "(" ++ lam term ++ ")"
         App _ _ => "(" ++ app term ++ ")"
         Var var => var

||| Return a list of all variables which appear free in the term 't'.
export total freeVars : Term -> List String
freeVars (Var v)   = [v]
freeVars (Lam v t) = delete v (freeVars t)
freeVars (App t u) = freeVars t `union` freeVars u

total isFreeIn : String -> Term -> Bool
isFreeIn var term = elem var (freeVars term)

||| Return all variables (free and bound) which appears in the term 't'.
export total vars : Term -> List String
vars (Var v)   = [v]
vars (Lam v t) = v :: vars t
vars (App t u) = vars t ++ vars u

||| Return a boolean to indicate whether the given term is reducible.
export total isRedex : Term -> Bool
isRedex (App (Lam _ _) _) = True
isRedex (App e1 e2)       = isRedex e1 || isRedex e2
isRedex (Lam _ e1)        = isRedex e1
isRedex _                 = False

another : String -> String
another name =
  case unpack name of
       (c :: [])      => if 'a' <= c && c < 'z'
                            then pack [succ c]
                            else pack (c :: '0' :: [])
       (b :: c :: []) => if '0' <= c && c < '9'
                            then pack (b :: succ c :: [])
                            else name ++ "'"
       _              => name ++ "'"

--fresh : Term -> Term -> String -> String
--fresh expr term = diff where
fresh : Term -> String -> String
fresh expr = diff where
  names : List String
  --names = union (freeVars expr) (vars term)
  names = freeVars expr 
  diff : String -> String
  diff x = let x' = another x in if x' `elem` names then diff x' else x'

alphaConvert : String -> String -> Term -> Term
alphaConvert from to term = 
  case term of
       (Var v)     => Var (if v == from then to else v)
       (App e1 e2) => App (alphaConvert from to e1) (alphaConvert from to e2)
       (Lam x e)   => Lam (if x == from then to else x) (alphaConvert from to e)

||| Perform the substitution `s[ n := e ]`.
||| @n a variable to substitute for
||| @e the term that the variable 'n' will be replaced with
||| @s the original term
export substitute : (n : String) -> (e : Term) -> (s : Term) -> Term
substitute var expr = subst where
  subst : Term -> Term
  subst (Var v)     = if var == v then expr else Var v
  subst (App e1 e2) = App (subst e1) (subst e2)
  subst (Lam x e) with (x == var)
    | True  = Lam x e -- If the variable we are susbstituting for is re-bound
    | False = if x `isFreeIn` expr
                 then let x' = fresh expr x
                          e' = alphaConvert x x' e in
                      Lam x' (subst e')
                 else Lam x  (subst e)

||| Beta-reduction in /normal order/, defined in terms of 'substitute'.
export reduct : (e : Term) -> Term
reduct (App (Lam v t) s) = substitute v s t
reduct (Lam v t) = Lam v (reduct t)
reduct (App t u) = if isRedex t 
                      then App (reduct t) u
                      else App t (reduct u)
reduct term = term
