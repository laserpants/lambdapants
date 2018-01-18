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

mutual
  export appl : Parser Term
  appl = do
    terms <- some (spaces *> expr)
    pure (foldl1 App terms)
  expr : Parser Term
  expr = map Var symbol  -- x
    <|>| lambda appl     -- \x.M
    <|>| parens appl     -- (M)

export
term : Parser Term
term = appl <* spaces <* eof
