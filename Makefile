UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
  OPEN := open
endif
ifeq ($(UNAME),Linux)
  OPEN := xdg-open
endif

OPAM=opam
EXEC=${OPAM} config exec
DUNE=${EXEC} jbuilder --

.PHONY: all build clean doc test top

all: build

build:
	@${DUNE} build @install

clean:
	@${DUNE} clean

doc:
	@${DUNE} build @doc
ifdef OPEN
	${OPEN} _build/default/_doc/tt/index.html
endif

test:
	@${DUNE} build @runtest

top:
	@${DUNE} utop src/lib
