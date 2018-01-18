# lambdapants

```
idris Main.idr -p lightyear -p effects -p baseline -o lambdapants
```

(Requires the [lightyear](https://github.com/ziman/lightyear) and [baseline](https://github.com/laserpants/baseline-idris) packages to be installed.)

### Features

- [x] Readline-like history and tab completion (using the [baseline-idris](https://github.com/laserpants/baseline-idris) package)
- [x] Normal order evaluation
- [x] Applicative order evaluation
- [ ] Other evaluation strategies
- [x] Fancy ANSI-color coded output :rainbow:
- [x] Church encoding of natural numbers
- [x] Built-in SKI combinators and other standard terms (`plus`, `succ`, `true`, `false`, `pair`, `fact`, `Y`, etc.) :ski:
- [ ] Documentation
- [ ] Command-line options
- [x] Support for shell commands (`:! <command>`)
- [x] Chuck Norris joke in error message
- [x] Tests
- [ ] More tests
