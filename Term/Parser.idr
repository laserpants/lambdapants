module Term.Parser

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Term

name : Parser String
name = do
  head <- alphaNum
  tail <- many (alphaNum <|> char '\'')
  let name = head :: tail
  pure (pack name)

lambda : (body : Parser Term) -> Parser Term
lambda term = do
  char '\\'
  var  <- name
  char '.'
  body <- term
  pure (Lam var body)

export term : Parser Term
term = do
  terms <- some (spaces *> expr)
  pure (foldl1 App terms)
where
  expr : Parser Term
  expr = map Var name  -- x
    <|>| lambda term   -- \x.M
    <|>| parens term   -- (M)
