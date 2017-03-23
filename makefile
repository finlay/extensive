IMAGE := docker.dragonfly.co.nz/finlay/extensive

SRC := $(shell find src -name "*.hs")
HASDOCKER ?= $(shell which docker-engine || which docker)
RUN := $(if $(HASDOCKER), docker run --net host --rm -u $$(id -u):$$(id -g) -e STACK_ROOT=/work -v $$PWD:/work -w /work $(IMAGE),) 

inverse.html: inverse
	./$< --output $@

inverse: inverse.hs $(SRC)
	$(RUN) bash -c 'stack --local-bin-path . install'

docker:
	docker build -t $(IMAGE) .
