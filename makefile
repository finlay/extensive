IMAGE := docker.dragonfly.co.nz/finlay/extensive:v2

SRC := $(shell find src -name "*.hs")
HASDOCKER ?= $(shell which docker-engine || which docker)
RUN := $(if $(HASDOCKER), docker run --net host --rm -v $$PWD:/work -w /work $(IMAGE),) 

DATE := $(shell date +"%Y-%m-%d")

all: inverse.csv

inverse.csv: inverse
	./$< --csv $@
	mv $@ timings/$(DATE)-$@
	Rscript plot.r timings/$(DATE)-$@	

inverse: inverse.hs $(SRC) extensive.cabal
	$(RUN) bash -c 'stack --allow-different-user --local-bin-path . install'

centre-exe: centre.hs $(SRC) extensive.cabal
	$(RUN) bash -c 'stack --allow-different-user --local-bin-path . install'

interact:
	docker run -it --net host --rm -v $$PWD:/work -w /work $(IMAGE) bash
docker:
	docker build -t $(IMAGE) .
docker-push:
	docker push $(IMAGE) 
