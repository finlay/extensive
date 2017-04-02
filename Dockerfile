FROM haskell:8.0.2
MAINTAINER finlay.thompson@gmail.com


COPY extensive.cabal extensive.cabal
COPY stack.yaml stack.yaml
COPY Setup.hs Setup.hs

RUN stack build --only-dependencies

RUN apt-get update \ 
	&& apt-get install -y --no-install-recommends \
		make r-cran-ggplot2 \
	&& rm -rf /var/lib/apt/lists/*

