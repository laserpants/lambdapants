module Test

import Command
import Command.Parser
import Lightyear
import Lightyear.Strings
import Term

export test : IO ()
test = do
  putStrLn (show (parse command "env"        == Right (Right (Env Nothing))))
  putStrLn (show (parse command "env  "      == Right (Right (Env Nothing))))
  putStrLn (show (isLeft (parse command "envxx")))
  putStrLn (show (parse command "env mu"     == Right (Right (Env (Just "mu")))))
  putStrLn (show (parse command "env   baz"  == Right (Right (Env (Just "baz")))))

  putStrLn (show (parse command "h "         == Right (Right Help)))
  putStrLn (show (parse command "h"          == Right (Right Help)))
  putStrLn (show (parse command "help  "     == Right (Right Help)))
  putStrLn (show (parse command "help"       == Right (Right Help)))
  putStrLn (show (isLeft (parse command "hp")))
  putStrLn (show (isLeft (parse command "helpo")))

  putStrLn (show (parse command "aq X Y"     == Right (Right (AlphaEq (Var "X") (Var "Y")))))
  putStrLn (show (isLeft (parse command "aqX Y")))
  putStrLn (show (parse command "aq X"       == Right (Left "Usage is :aq <term> <term>")))
  putStrLn (show (parse command "aq"         == Right (Left "Usage is :aq <term> <term>")))
  putStrLn (show (parse command "aq "        == Right (Left "Usage is :aq <term> <term>")))
  putStrLn (show (parse command "aq (\\x.x)" == Right (Left "Usage is :aq <term> <term>")))

  putStrLn (show (parse command "eq X Y"     == Right (Right (Eq (Var "X") (Var "Y")))))
  putStrLn (show (isLeft (parse command "eqX Y")))
  putStrLn (show (parse command "eq X"       == Right (Left "Usage is :eq <term> <term>")))

  putStrLn (show (parse command "reduce"     == Right (Left "Usage is :r[educe] <term>")))
  putStrLn (show (parse command "reduce  "   == Right (Left "Usage is :r[educe] <term>")))
  putStrLn (show (parse command "r"          == Right (Left "Usage is :r[educe] <term>")))
  putStrLn (show (isLeft (parse command "reducex")))
  putStrLn (show (parse command "reduce X"   == Right (Right (Reduce (Var "X")))))

  putStrLn (show (parse command "lookup"     == Right (Left "Usage is :l[ookup] <term>")))
  putStrLn (show (parse command "lookup  "   == Right (Left "Usage is :l[ookup] <term>")))
  putStrLn (show (parse command "l"          == Right (Left "Usage is :l[ookup] <term>")))
  putStrLn (show (isLeft (parse command "lookupx")))
  putStrLn (show (parse command "lookup X"   == Right (Right (Lookup (Var "X")))))
  putStrLn (show (parse command "lookup X Y" == Right (Right (Lookup (App (Var "X") (Var "Y"))))))
  putStrLn (show (parse command "l (X Y)"    == Right (Right (Lookup (App (Var "X") (Var "Y"))))))

  putStrLn (show (parse command "delete "    == Right (Left "Usage is :d[elete] <symbol>")))
  putStrLn (show (parse command "delete"     == Right (Left "Usage is :d[elete] <symbol>")))
  putStrLn (show (parse command "delete  "   == Right (Left "Usage is :d[elete] <symbol>")))
  putStrLn (show (parse command "d "         == Right (Left "Usage is :d[elete] <symbol>")))
  putStrLn (show (parse command "d"          == Right (Left "Usage is :d[elete] <symbol>")))
  putStrLn (show (isLeft (parse command "dz")))
  putStrLn (show (isLeft (parse command "del del")))
  putStrLn (show (isLeft (parse command "del wat")))
  putStrLn (show (parse command "d baz"      == Right (Right (Delete "baz"))))
  putStrLn (show (parse command "delete baz" == Right (Right (Delete "baz"))))
  putStrLn (show (parse command "delete fez_baz" == Right (Right (Delete "fez_baz"))))
  putStrLn (show (parse command "d fez_baz"  == Right (Right (Delete "fez_baz"))))

  putStrLn (show (parse command "limit 3"    == Right (Right (Limit 3))))
  putStrLn (show (parse command "limit 300"  == Right (Right (Limit 300))))
  putStrLn (show (parse command "limit"      == Right (Left "Usage is :limit <number>")))
  putStrLn (show (parse command "limit "     == Right (Left "Usage is :limit <number>")))
  putStrLn (show (parse command "limit foo"  == Right (Left "Usage is :limit <number>")))
  putStrLn (show (parse command "limit 30x"  == Right (Left "Usage is :limit <number>")))
  putStrLn (show (parse command "limit f3"   == Right (Left "Usage is :limit <number>")))
  putStrLn (show (isLeft (parse command "limits")))

  putStrLn (show (parse command "q "         == Right (Right Quit)))
  putStrLn (show (parse command "q"          == Right (Right Quit)))
  putStrLn (show (parse command "quit "      == Right (Right Quit)))
  putStrLn (show (parse command "quit"       == Right (Right Quit)))
  putStrLn (show (isLeft (parse command "qu")))
  putStrLn (show (isLeft (parse command "quitquit")))

  putStrLn (show (isLeft (parse command "saved")))
  putStrLn (show (parse command "save"             == Right (Left "Usage is :s[ave] <symbol> <term>")))
  putStrLn (show (parse command "save "            == Right (Left "Usage is :s[ave] <symbol> <term>")))
  putStrLn (show (parse command "save baz"         == Right (Left "Usage is :s[ave] <symbol> <term>")))
  putStrLn (show (parse command "save baz \\x.("   == Right (Left "Usage is :s[ave] <symbol> <term>")))
  putStrLn (show (parse command "save foo foo"     == Right (Right (Save "foo" (Var "foo")))))
  putStrLn (show (parse command "save id \\x.x"    == Right (Right (Save "id" (Lam "x" (Var "x"))))))
  putStrLn (show (parse command "save id    \\x.x" == Right (Right (Save "id" (Lam "x" (Var "x"))))))
  putStrLn (show (parse command "save x_x (A B)"   == Right (Right (Save "x_x" (App (Var "A") (Var "B"))))))
  putStrLn (show (parse command "save  why (\\g.(\\x.g (x x)) (\\x.g (x x)))" == Right (Right (Save "why" (Lam "g" (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "g") (App (Var "x") (Var "x"))))))))))
