all: test-main


.PHONY: test test-main test-aux test-c-main test-c-aux test-hs-main test-hs-aux clean


RHOME = $(shell pkg-config --variable=rhome libR)
RFLAGS = $(shell pkg-config --cflags-only-I --libs-only-L --libs-only-l libR)
CFLAGS = -Wall $(RFLAGS)
HSFLAGS = -Wall -hidir build/hs -odir build/hs -threaded $(RFLAGS)


test: test-main test-aux

test-main: test-c-main test-hs-main

test-aux: test-c-aux test-hs-aux


test-c-main: build/c/embedR-main
	@echo "-----> Embedding R in C using the main thread..."
	R_HOME=$(RHOME) build/c/embedR-main <test/test.R

test-c-aux: build/c/embedR-aux
	@echo "-----> Embedding R in C using an auxiliary pthread..."
	R_HOME=$(RHOME) build/c/embedR-aux <test/test.R

test-hs-main: build/hs/embedR-main
	@echo "-----> Embedding R in Haskell using the main thread..."
	R_HOME=$(RHOME) build/hs/embedR-main <test/test.R

test-hs-aux: build/hs/embedR-aux
	@echo "-----> Embedding R in Haskell using an auxiliary OS thread..."
	R_HOME=$(RHOME) build/hs/embedR-aux <test/test.R


build/c/embedR-main: src/c/embedR.c
	@mkdir -p build/c
	gcc $(CFLAGS) -o build/c/embedR-main src/c/embedR.c

build/c/embedR-aux: src/c/embedR.c
	@mkdir -p build/c
	gcc -DAUX $(CFLAGS) -o build/c/embedR-aux src/c/embedR.c

build/hs/embedR-main: src/hs/embedR.hs
	@mkdir -p build/hs
	ghc $(HSFLAGS) -o build/hs/embedR-main src/hs/embedR.hs

build/hs/embedR-aux: src/hs/embedR.hs
	@mkdir -p build/hs
	ghc -DAUX $(HSFLAGS) -o build/hs/embedR-aux src/hs/embedR.hs


clean:
	rm -rf build
