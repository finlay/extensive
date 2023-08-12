IMAGE := docker.dragonfly.co.nz/finlay/extensive:v4

SRC := $(shell find src -name "*.hs")
HASDOCKER ?= $(shell which docker-engine || which docker)
RUN := $(if $(HASDOCKER), docker run --net host --rm -v $$PWD:/work -w /work $(IMAGE),)

DATE := $(shell date +"%Y-%m-%d")

all: timings/$(DATE)-inverse.csv timings/$(DATE)-inverse.pdf symmetric.pdf

%.csv: inverse
	mkdir -p timings
	rm -f $@
	./$< --csv $@

%.pdf: %.csv plot.r
	Rscript plot.r $<

inverse: inverse.hs $(SRC) extensive.cabal
	$(RUN) bash -c 'stack --allow-different-user --local-bin-path . install'

centre-exe: centre.hs $(SRC) extensive.cabal
	$(RUN) bash -c 'stack --allow-different-user --local-bin-path . install'

symmetric-exe: symmetric.hs $(SRC) extensive.cabal
	$(RUN) bash -c 'stack --allow-different-user --local-bin-path . install'

symmetric.pdf: symmetric.tex
	xelatex $<

local:
	docker run -it --rm --net=host --user=$$(id -u):$$(id -g) \
		-e DISPLAY=$$DISPLAY \
		-e HOME=/work \
		-e RUN= \
		-v$$(pwd):/work \
		-w /work/ \
	 	$(IMAGE) bash
docker:
	docker build -t $(IMAGE) .
docker-push:
	docker push $(IMAGE)
