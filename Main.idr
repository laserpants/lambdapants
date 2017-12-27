module Main

import Lightyear.Strings
import Term
import Term.Parser

main : IO ()
main = printLn (parse term "\\x.x")
