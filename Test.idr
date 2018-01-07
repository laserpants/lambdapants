module Test

import Lambdapants.Command
import Lambdapants.Command.Parser
import Lambdapants.Term
import Lightyear
import Lightyear.Strings

export test : IO ()
test = do
  putStrLn (show (parseCmd "env"        == Right (Env Nothing)))
  putStrLn (show (parseCmd "env  "      == Right (Env Nothing)))
  putStrLn (show (isLeft (parseCmd "envxx")))
  putStrLn (show (parseCmd "env mu"     == Right (Env (Just "mu"))))
  putStrLn (show (parseCmd "env   baz"  == Right (Env (Just "baz"))))

  putStrLn (show (parseCmd "h "         == Right Help))
  putStrLn (show (parseCmd "h"          == Right Help))
  putStrLn (show (parseCmd "help  "     == Right Help))
  putStrLn (show (parseCmd "help"       == Right Help))
  putStrLn (show (isLeft (parseCmd "hp")))
  putStrLn (show (isLeft (parseCmd "helpo")))
  putStrLn (show (parseCmd "? "         == Right Help))
  putStrLn (show (parseCmd "?"          == Right Help))
  putStrLn (show (isLeft (parseCmd "?elp")))

  putStrLn (show (parseCmd "aq X Y"     == Right (AlphaEq (Var "X") (Var "Y"))))
  putStrLn (show (isLeft (parseCmd "aqX Y")))
  putStrLn (show (parseCmd "aq X"       == Left "Usage is :aq <term> <term>"))
  putStrLn (show (parseCmd "aq"         == Left "Usage is :aq <term> <term>"))
  putStrLn (show (parseCmd "aq "        == Left "Usage is :aq <term> <term>"))
  putStrLn (show (parseCmd "aq (\\x.x)" == Left "Usage is :aq <term> <term>"))

  putStrLn (show (parseCmd "eq X Y"     == Right (Eq (Var "X") (Var "Y"))))
  putStrLn (show (isLeft (parseCmd "eqX Y")))
  putStrLn (show (parseCmd "eq X"       == Left "Usage is :eq <term> <term>"))

  putStrLn (show (parseCmd "reduce"     == Left "Usage is :reduce <term>"))
  putStrLn (show (parseCmd "reduce  "   == Left "Usage is :reduce <term>"))
  putStrLn (show (parseCmd "r"          == Left "Usage is :r <term>"))
  putStrLn (show (isLeft (parseCmd "reducex")))
  putStrLn (show (parseCmd "reduce X"   == Right (Reduce (Var "X"))))

  putStrLn (show (parseCmd "lookup"       == Left "Usage is :lookup <term>"))
  putStrLn (show (parseCmd "lookup  "     == Left "Usage is :lookup <term>"))
  putStrLn (show (parseCmd "l"            == Left "Usage is :l <term>"))
  putStrLn (show (isLeft (parseCmd "lookupx")))
  putStrLn (show (parseCmd "lookup X"     == Right (Lookup (Var "X"))))
  putStrLn (show (parseCmd "lookup (X Y)" == Right (Lookup (App (Var "X") (Var "Y")))))
  putStrLn (show (parseCmd "l (X Y)"      == Right (Lookup (App (Var "X") (Var "Y")))))

  putStrLn (show (parseCmd "delete "    == Left "Usage is :delete <symbol>"))
  putStrLn (show (parseCmd "delete"     == Left "Usage is :delete <symbol>"))
  putStrLn (show (parseCmd "delete  "   == Left "Usage is :delete <symbol>"))
  putStrLn (show (parseCmd "d "         == Left "Usage is :d <symbol>"))
  putStrLn (show (parseCmd "d"          == Left "Usage is :d <symbol>"))
  putStrLn (show (isLeft (parseCmd "dz")))
  putStrLn (show (isLeft (parseCmd "del del")))
  putStrLn (show (isLeft (parseCmd "del wat")))
  putStrLn (show (parseCmd "d baz"      == Right (Delete "baz")))
  putStrLn (show (parseCmd "delete baz" == Right (Delete "baz")))
  putStrLn (show (parseCmd "delete fez_baz" 
                                        == Right (Delete "fez_baz")))
  putStrLn (show (parseCmd "d fez_baz"  == Right (Delete "fez_baz")))

  putStrLn (show (parseCmd "limit 3"    == Right (Limit 3)))
  putStrLn (show (parseCmd "limit 300"  == Right (Limit 300)))
  putStrLn (show (parseCmd "limit"      == Left "Usage is :limit <number>"))
  putStrLn (show (parseCmd "limit "     == Left "Usage is :limit <number>"))
  putStrLn (show (parseCmd "limit foo"  == Left "Usage is :limit <number>"))
  putStrLn (show (parseCmd "limit 30x"  == Left "Usage is :limit <number>"))
  putStrLn (show (parseCmd "limit f3"   == Left "Usage is :limit <number>"))
  putStrLn (show (isLeft (parseCmd "limits")))

  putStrLn (show (parseCmd "q "         == Right Quit))
  putStrLn (show (parseCmd "q"          == Right Quit))
  putStrLn (show (parseCmd "quit "      == Right Quit))
  putStrLn (show (parseCmd "quit"       == Right Quit))
  putStrLn (show (isLeft (parseCmd "qu")))
  putStrLn (show (isLeft (parseCmd "quitquit")))

  putStrLn (show (isLeft (parseCmd "saved")))
  putStrLn (show (parseCmd "save"               == Left "Usage is :save <symbol> <term>"))
  putStrLn (show (parseCmd "save "              == Left "Usage is :save <symbol> <term>"))
  putStrLn (show (parseCmd "save baz"           == Left "Usage is :save <symbol> <term>"))
  putStrLn (show (parseCmd "save baz \\x.("     == Left "Usage is :save <symbol> <term>"))
  putStrLn (show (parseCmd "save foo foo"       == Right (Save "foo" (Var "foo"))))
  putStrLn (show (parseCmd "save id (\\x.x)"    == Right (Save "id" (Lam "x" (Var "x")))))
  putStrLn (show (parseCmd "save id    (\\x.x)" == Right (Save "id" (Lam "x" (Var "x")))))
  putStrLn (show (parseCmd "save x_x (A B)"     == Right (Save "x_x" (App (Var "A") (Var "B")))))
  putStrLn (show (parseCmd "save  why (\\g.(\\x.g (x x)) (\\x.g (x x)))" 
                                                == Right (Save "why" (Lam "g" (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))))))))
