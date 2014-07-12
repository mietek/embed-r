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


all: build

.PHONY: build run-c run-ghc run-ghci test test-c test-ghc test-ghci clean


build: build/embedR-c build/embedR-ghc build/$(SHARED_ALT_R)

build/embedR-c: build/altR.o src/embedR.c
	gcc $(R_CFLAGS) $(R_LIBS) -Wall -o $@ -std=c99 $^

build/embedR-ghc: build/altR.o src/embedR.hs
	ghc $(R_LIBS) -Wall -hidir build -odir build -o $@ -threaded $^

build/$(SHARED_ALT_R): build/altR.o
	gcc $(R_LIBS) -shared -o $@ -std=c99 $^

build/altR.o: src/altR.c
	@mkdir -p build
	gcc $(R_CFLAGS) -Wall -c -fPIC -o $@ -std=c99 $^


run-c: build/embedR-c
	@echo "-----> Embedding R in C"
	R_HOME=$(R_HOME) build/embedR-c

run-ghc: build/embedR-ghc
	@echo "-----> Embedding R in Haskell"
	R_HOME=$(R_HOME) build/embedR-ghc

run-ghci: build/$(SHARED_ALT_R)
	@echo "-----> Embedding R in Haskell using GHCi"
	R_HOME=$(R_HOME) ghci -Wall -fno-ghci-sandbox -hidir build -odir build $^


test: test-c test-ghc test-ghci

test-c: build/embedR-c
	@echo "-----> Embedding R in C"
	R_HOME=$(R_HOME) build/embedR-c <test/test.R

test-ghc: build/embedR-ghc
	@echo "-----> Embedding R in Haskell"
	R_HOME=$(R_HOME) build/embedR-ghc <test/test.R

test-ghci: build/$(SHARED_ALT_R)
	@echo "-----> Embedding R in Haskell using GHCi"
	R_HOME=$(R_HOME) ghci -Wall -fno-ghci-sandbox -hidir build -odir build $^ -ghci-script test/test.ghci <test/test.R


clean:
	rm -rf build
