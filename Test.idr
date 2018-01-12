module Test

import Lambdapants.Command
import Lambdapants.Command.Parser
import Lambdapants.Term
import Lambdapants.Term.Parser
import Lambdapants.Term.Reduction
import Lightyear
import Lightyear.Strings

partial parseUnsafe_ : String -> Term
parseUnsafe_ input =
  case parse term input of
       Right t => t

export test : IO ()
test = do

  -- Test reductions

  let t0 = parseUnsafe_ "(\\x.\\y.x y) y"
  putStrLn (show (parseUnsafe_ "\\z.y z" `alphaEq` nor t0))

  let t0_1 = parseUnsafe_ "(\\x.\\y.\\z.x y z) z"
  putStrLn (show (parseUnsafe_ "\\y.\\z0.z y z0" `alphaEq` nor t0_1))

  let t0_2 = parseUnsafe_ "(\\x.\\y.\\z.\\a.x y z a) (z a)"
  putStrLn (show (parseUnsafe_ "\\y.\\z0.\\a0.(z a) y z0 a0" `alphaEq` nor t0_2))

  let t1 = parseUnsafe_ "(\\a.\\b.a) c ((\\d.e) d)"
  let t2 = parseUnsafe_ "(\\b.c) ((\\d.e) d)"
  putStrLn (show (t2 `alphaEq` nor t1))
  let t3 = parseUnsafe_ "c"
  putStrLn (show (t3 `alphaEq` nor t2))
  --
  let t4 = parseUnsafe_ "(\\a.\\b.a) c ((\\d.e) d)"
  let t5 = parseUnsafe_ "(\\a.\\b.a) c e"
  putStrLn (show (t5 `alphaEq` aor t4))
  let t6 = parseUnsafe_ "(\\b.c) e"
  putStrLn (show (t6 `alphaEq` aor t5))
  putStrLn (show (t3 `alphaEq` aor t6))
  --
  let t7 = parseUnsafe_ "(\\x.a)((\\x.x x)(\\y.y y))"
  putStrLn (show (parseUnsafe_ "a" `alphaEq` nor t7))
  --
  let t8 = parseUnsafe_ "(\\x.a)((\\y.y y)(\\y.y y))"
  putStrLn (show (t8 `alphaEq` aor t7))
  --
  let t9 = parseUnsafe_ "((\\a.x) ((\\a.a a)(\\a.((\\b.a b) a))))"
  putStrLn (show (Var "x" `alphaEq` nor t9))
  --
  let t10 = parseUnsafe_ "(\\x.\\i0.y) c (\\z.(\\w.(\\b.(\\a.a) c) z) f)"
  let t11 = parseUnsafe_ "(\\x.y) (\\z.(\\w.(\\b.(\\a.a) c) z) f)"
  putStrLn (show (t11 `alphaEq` nor t10))
  putStrLn (show (Var "y" `alphaEq` nor t11))
  --
  let t12 = parseUnsafe_ "((\\a.x) ((\\a.a a) (\\a.a a)))"
  putStrLn (show (t12 `alphaEq` aor t9))
  putStrLn (show (t12 `alphaEq` aor (aor t9)))
  --
  let t13 = parseUnsafe_ "((\\x.\\x.y) c (\\z.(\\w.(\\b.(\\a.a) c) z) f))"
  let t14 = parseUnsafe_ "(\\x.\\i0.y) c (\\z.(\\b.(\\a.a) c) z)"
  putStrLn (show (t14 `alphaEq` aor t13))
  let t15 = parseUnsafe_ "(\\x.\\i0.y) c (\\z.(\\a.a) c)"
  putStrLn (show (t15 `alphaEq` aor t14))
  let t16 = parseUnsafe_ "(\\x.\\i0.y) c (\\z.c)"
  putStrLn (show (t16 `alphaEq` aor t15))
  let t17 = parseUnsafe_ "(\\x.y) (\\z.c)"
  putStrLn (show (t17 `alphaEq` aor t16))
  let t18 = parseUnsafe_ "y"
  putStrLn (show (t18 `alphaEq` aor t17))
  --


--  -- Test repl expressions
--
--  putStrLn (show (parseCmd "env"        == Right (Env Nothing)))
--  putStrLn (show (parseCmd "env  "      == Right (Env Nothing)))
--  putStrLn (show (isLeft (parseCmd "envxx")))
--  putStrLn (show (parseCmd "env mu"     == Right (Env (Just "mu"))))
--  putStrLn (show (parseCmd "env   baz"  == Right (Env (Just "baz"))))
--
--  putStrLn (show (parseCmd "h "         == Right Help))
--  putStrLn (show (parseCmd "h"          == Right Help))
--  putStrLn (show (parseCmd "help  "     == Right Help))
--  putStrLn (show (parseCmd "help"       == Right Help))
--  putStrLn (show (isLeft (parseCmd "hp")))
--  putStrLn (show (isLeft (parseCmd "helpo")))
--  putStrLn (show (parseCmd "? "         == Right Help))
--  putStrLn (show (parseCmd "?"          == Right Help))
--  putStrLn (show (isLeft (parseCmd "?elp")))
--
--  putStrLn (show (parseCmd "aq X Y"     == Right (AlphaEq (Var "X") (Var "Y"))))
--  putStrLn (show (isLeft (parseCmd "aqX Y")))
--  putStrLn (show (parseCmd "aq X"       == Left "Usage is :aq <term> <term>"))
--  putStrLn (show (parseCmd "aq"         == Left "Usage is :aq <term> <term>"))
--  putStrLn (show (parseCmd "aq "        == Left "Usage is :aq <term> <term>"))
--  putStrLn (show (parseCmd "aq (\\x.x)" == Left "Usage is :aq <term> <term>"))
--
--  putStrLn (show (parseCmd "eq X Y"     == Right (Eq (Var "X") (Var "Y"))))
--  putStrLn (show (isLeft (parseCmd "eqX Y")))
--  putStrLn (show (parseCmd "eq X"       == Left "Usage is :eq <term> <term>"))
--
--  putStrLn (show (parseCmd "reduce"     == Left "Usage is :reduce <term>"))
--  putStrLn (show (parseCmd "reduce  "   == Left "Usage is :reduce <term>"))
--  putStrLn (show (parseCmd "r"          == Left "Usage is :r <term>"))
--  putStrLn (show (isLeft (parseCmd "reducex")))
--  putStrLn (show (parseCmd "reduce X"   == Right (Reduce (Var "X"))))
--
--  putStrLn (show (parseCmd "lookup"       == Left "Usage is :lookup <term>"))
--  putStrLn (show (parseCmd "lookup  "     == Left "Usage is :lookup <term>"))
--  putStrLn (show (parseCmd "l"            == Left "Usage is :l <term>"))
--  putStrLn (show (isLeft (parseCmd "lookupx")))
--  putStrLn (show (parseCmd "lookup X"     == Right (Lookup (Var "X"))))
--  putStrLn (show (parseCmd "lookup (X Y)" == Right (Lookup (App (Var "X") (Var "Y")))))
--  putStrLn (show (parseCmd "l (X Y)"      == Right (Lookup (App (Var "X") (Var "Y")))))
--
--  putStrLn (show (parseCmd "delete "    == Left "Usage is :delete <symbol>"))
--  putStrLn (show (parseCmd "delete"     == Left "Usage is :delete <symbol>"))
--  putStrLn (show (parseCmd "delete  "   == Left "Usage is :delete <symbol>"))
--  putStrLn (show (parseCmd "d "         == Left "Usage is :d <symbol>"))
--  putStrLn (show (parseCmd "d"          == Left "Usage is :d <symbol>"))
--  putStrLn (show (isLeft (parseCmd "dz")))
--  putStrLn (show (isLeft (parseCmd "del del")))
--  putStrLn (show (isLeft (parseCmd "del wat")))
--  putStrLn (show (parseCmd "d baz"      == Right (Delete "baz")))
--  putStrLn (show (parseCmd "delete baz" == Right (Delete "baz")))
--  putStrLn (show (parseCmd "delete fez_baz"
--                                        == Right (Delete "fez_baz")))
--  putStrLn (show (parseCmd "d fez_baz"  == Right (Delete "fez_baz")))
--
--  putStrLn (show (parseCmd "limit 3"    == Right (Limit 3)))
--  putStrLn (show (parseCmd "limit 300"  == Right (Limit 300)))
--  putStrLn (show (parseCmd "limit"      == Left "Usage is :limit <number>"))
--  putStrLn (show (parseCmd "limit "     == Left "Usage is :limit <number>"))
--  putStrLn (show (parseCmd "limit foo"  == Left "Usage is :limit <number>"))
--  putStrLn (show (parseCmd "limit 30x"  == Left "Usage is :limit <number>"))
--  putStrLn (show (parseCmd "limit f3"   == Left "Usage is :limit <number>"))
--  putStrLn (show (isLeft (parseCmd "limits")))
--
--  putStrLn (show (parseCmd "q "         == Right Quit))
--  putStrLn (show (parseCmd "q"          == Right Quit))
--  putStrLn (show (parseCmd "quit "      == Right Quit))
--  putStrLn (show (parseCmd "quit"       == Right Quit))
--  putStrLn (show (isLeft (parseCmd "qu")))
--  putStrLn (show (isLeft (parseCmd "quitquit")))
--
--  putStrLn (show (isLeft (parseCmd "saved")))
--  putStrLn (show (parseCmd "save"               == Left "Usage is :save <symbol> <term>"))
--  putStrLn (show (parseCmd "save "              == Left "Usage is :save <symbol> <term>"))
--  putStrLn (show (parseCmd "save baz"           == Left "Usage is :save <symbol> <term>"))
--  putStrLn (show (parseCmd "save baz \\x.("     == Left "Usage is :save <symbol> <term>"))
--  putStrLn (show (parseCmd "save foo foo"       == Right (Save "foo" (Var "foo"))))
--  putStrLn (show (parseCmd "save id (\\x.x)"    == Right (Save "id" (Lam "x" (Var "x")))))
--  putStrLn (show (parseCmd "save id    (\\x.x)" == Right (Save "id" (Lam "x" (Var "x")))))
--  putStrLn (show (parseCmd "save x_x (A B)"     == Right (Save "x_x" (App (Var "A") (Var "B")))))
--  putStrLn (show (parseCmd "save  why (\\g.(\\x.g (x x)) (\\x.g (x x)))"
--                                                == Right (Save "why" (Lam "g" (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))))))))
