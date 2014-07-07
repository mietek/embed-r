all: crun hsrun


.PHONY: crun crun-fork hsrun hsrun-fork clean


RHOME = $(shell pkg-config --variable=rhome libR)
RFLAGS = $(shell pkg-config --cflags --libs libR)
CFLAGS = -Wall $(RFLAGS)
HSFLAGS = -Wall -threaded $(RFLAGS)


crun: cembedR
	R_HOME=$(RHOME) ./cembedR < R.cmd

crun-fork: cembedR-fork
	R_HOME=$(RHOME) ./cembedR-fork < R.cmd

cembedR: cembedR.c
	gcc $(CFLAGS) -o cembedR cembedR.c

cembedR-fork: cembedR.c
	gcc -DFORK $(CFLAGS) -o cembedR-fork cembedR.c


hsrun: hsembedR
	R_HOME=$(RHOME) ./hsembedR < R.cmd

hsrun-fork: hsembedR-fork
	R_HOME=$(RHOME) ./hsembedR-fork < R.cmd

hsembedR: hsembedR.hs
	ghc $(HSFLAGS) -o hsembedR hsembedR.hs

hsembedR-fork: hsembedR.hs
	ghc -DFORK $(HSFLAGS) -o hsembedR-fork hsembedR.hs


clean:
	rm -f cembedR cembedR-fork hsembedR hsembedR-fork hsembedR.hi hsembedR.o
