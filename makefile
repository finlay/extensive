IMAGE := docker.dragonfly.co.nz/finlay/extensive

SRC := $(shell find src -name "*.hs")
HASDOCKER ?= $(shell which docker-engine || which docker)
RUN := $(if $(HASDOCKER), docker run --net host --rm -v $$PWD:/work -w /work $(IMAGE),) 

all: inverse.csv

inverse.csv: inverse
	./$< --csv $@

inverse: inverse.hs $(SRC) extensive.cabal
	$(RUN) bash -c 'stack --allow-different-user --local-bin-path . install'

docker:
	docker build -t $(IMAGE) .
