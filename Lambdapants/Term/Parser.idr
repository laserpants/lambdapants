module Lambdapants.Term.Parser

import Lambdapants.Term
import Lightyear
import Lightyear.Char
import Lightyear.Strings

export
atom : Parser String
atom = pack <$> some (alphaNum <|> char '_')

lambda : (body : Parser Term) -> Parser Term
lambda term = do
  char '\\'
  var  <- atom
  char '.'
  body <- term
  pure (Lam var body)

export
term : Parser Term
term = do
  terms <- some (spaces *> expr)
  pure (foldl1 App terms)
where
  expr : Parser Term
  expr = map Var atom  -- x
    <|>| lambda term   -- \x.M
    <|>| parens term   -- (M)
