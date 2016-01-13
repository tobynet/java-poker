# Syakyo of Poker in Haskell

* For `Syakyo`, refer to https://en.wikipedia.org/wiki/Sutra_copying
* For `Poker in Haskell`, refer to http://tune.hateblo.jp/entry/2015/05/12/023112

## Requirements

* Haskell compiler(ghc 7.8.4 or lator)
* The `random-shuffle` package 
    https://hackage.haskell.org/package/random-shuffle

## Install and try

Use [stack]: 

```bash
$ stack update && stack install java-poker
$ hash -r
$ java-poker
```

Or use [cabal]:

```bash
$ cabal update && cabal install java-poker
$ hash -r
$ java-poker
```

Or use [stack] and build master branch in git: 

```bash
$ git clone https://github.com/tobynet/java-poker.git
$ cd java-poker
$ stack build
$ stack exec java-poker
```

[stack]: https://www.stackage.org/
[cabal]: https://www.haskell.org/cabal/
