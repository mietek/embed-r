R_HOME = $(shell pkg-config --variable=rhome libR)
R_CFLAGS = $(shell pkg-config --cflags-only-I libR)
R_LIBS = $(shell pkg-config --libs-only-L libR) $(shell pkg-config --libs-only-l libR)

UNAME_S = $(shell sh -c 'uname -s 2>/dev/null || echo not')
UNAME_O = $(shell sh -c 'uname -o 2>/dev/null || echo not')

ifeq ($(UNAME_S),Linux)
SHARED_ALT_R = altR.so
endif

ifeq ($(UNAME_S),Darwin)
SHARED_ALT_R = altR.dylib
endif

ifeq ($(UNAME_O),Cygwin)
SHARED_ALT_R = altR.dll
endif


all: test

.PHONY: test test-c test-hs test-hs-i test-c-aux test-hs-aux test-hs-i-aux clean

test: test-c test-hs test-hs-i

test-aux: test-c-aux test-hs-aux


test-c: build/embedR-c
	@echo "-----> Embedding R in C..."
	R_HOME=$(R_HOME) build/embedR-c <test/test.R

test-c-aux: build/embedR-c-aux
	@echo "-----> Embedding R in C using an auxiliary thread..."
	R_HOME=$(R_HOME) build/embedR-c-aux <test/test.R

test-hs: build/embedR-hs
	@echo "-----> Embedding R in Haskell..."
	R_HOME=$(R_HOME) build/embedR-hs <test/test.R

test-hs-aux: build/embedR-hs-aux
	@echo "-----> Embedding R in Haskell using an auxiliary thread..."
	R_HOME=$(R_HOME) build/embedR-hs-aux <test/test.R

test-hs-i: build/$(SHARED_ALT_R)
	@echo "-----> Embedding R in Haskell using GHCi..."
	R_HOME=$(R_HOME) ghci -Wall -fno-ghci-sandbox -hidir build -odir build $^ -ghci-script test/test.ghci <test/test.R

test-hs-i-aux: build/$(SHARED_ALT_R)
	@echo "-----> Embedding R in Haskell using an auxilliary thread within GHCi..."
	R_HOME=$(R_HOME) ghci -Wall -hidir build -odir build $^ -ghci-script test/test.ghci <test/test.R


build/embedR-c: build/altR.o src/embedR.c
	gcc $(R_CFLAGS) $(R_LIBS) -Wall -o $@ -std=c99 $^

build/embedR-c-aux: build/altR.o src/embedR.c
	gcc $(R_CFLAGS) $(R_LIBS) -DAUX -Wall -o $@ -std=c99 $^

build/embedR-hs: build/altR.o src/embedR.hs
	ghc $(R_LIBS) -Wall -debug -hidir build -odir build -o $@ -threaded $^

build/embedR-hs-aux: build/altR.o src/embedR.hs
	ghc $(R_LIBS) -DAUX -Wall -debug -hidir build -odir build -o $@ -threaded $^


build/$(SHARED_ALT_R): build/altR.o
	gcc $(R_LIBS) -shared -o $@ -std=c99 $^


build/altR.o: src/altR.c
	@mkdir -p build
	gcc $(R_CFLAGS) -Wall -c -fPIC -o $@ -std=c99 $^


clean:
	rm -rf build
