## Extensive - experiments in linear algebra in haskell

This repo collects a range of random musing regarding the encoding of linear
algebra in haskell. The idea involve building on Ander Kock's
[Commutative monads as a theory of distributions](http://tildeweb.au.dk/au76680/CMTD.pdf).

Essentially, the monad of co-vectors, over `a` form a vector space.

```,haskell
newtype T a = T ((a -> R) -> R)
```

## How to run things

The project is set to run using Cabal

```,bash
user@machine:extensive $ cabal install
user@machine:extensive $ cabal repl


```


This will get you to a command prompt in the GHCi REPL, with the extensive libraries loaded.
