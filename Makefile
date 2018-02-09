OPAM=opam
EXEC=${OPAM} config exec
DUNE=${EXEC} jbuilder --

.PHONY: all build clean run top

all: build

build:
	@${DUNE} build @install

clean:
	@${DUNE} clean

top:
	@${DUNE} utop src/lib
