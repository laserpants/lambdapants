module Lambdapants.Term.Parser

import Lambdapants.Term
import Lightyear
import Lightyear.Char
import Lightyear.Strings

export
symbol : Parser String
symbol = pack <$> some (alphaNum <|> char '_')

export
lambda : (body : Parser Term) -> Parser Term
lambda term = do
  char '\\'
  var  <- symbol
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
  expr = map Var symbol  -- x
    <|>| lambda term     -- \x.M
    <|>| parens term     -- (M)
