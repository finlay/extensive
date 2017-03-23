FROM haskell:8.0.2
MAINTAINER finlay.thompson@gmail.com


COPY extensive.cabal extensive.cabal
COPY stack.yaml stack.yaml
COPY Setup.hs Setup.hs

RUN stack build --only-dependencies

