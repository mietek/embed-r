all: test-main


.PHONY: test test-main test-aux test-c-main test-c-aux test-hs-main test-hs-aux clean


R_HOME = $(shell pkg-config --variable=rhome libR)
C_FLAGS = -Wall $(shell pkg-config --cflags libR)
LIBS = $(shell pkg-config --libs-only-L --libs-only-l libR)
HS_FLAGS = -Wall -hidir build/hs -odir build/hs


test: test-main test-aux

test-main: test-c-main test-hs-main test-hs-main-i

test-aux: test-c-aux test-hs-aux


test-c-main: build/c/embedR-main
	@echo "-----> Embedding R in C using the main thread..."
	R_HOME=$(R_HOME) build/c/embedR-main <test/test.R

test-c-aux: build/c/embedR-aux
	@echo "-----> Embedding R in C using an auxiliary thread..."
	R_HOME=$(R_HOME) build/c/embedR-aux <test/test.R

test-hs-main: build/hs/embedR-main
	@echo "-----> Embedding R in Haskell using the main thread..."
	R_HOME=$(R_HOME) build/hs/embedR-main <test/test.R

test-hs-aux: build/hs/embedR-aux
	@echo "-----> Embedding R in Haskell using an auxiliary thread..."
	R_HOME=$(R_HOME) build/hs/embedR-aux <test/test.R

test-hs-main-i:
	@echo "-----> Embedding R in Haskell using the main thread within GHCi..."
	R_HOME=$(R_HOME) ghci -fno-ghci-sandbox $(HS_FLAGS) $(LIBS) -ghci-script test/test.ghci <test/test.R

test-hs-aux-i:
	@echo "-----> Embedding R in Haskell using an auxilliary thread within GHCi..."
	R_HOME=$(R_HOME) ghci $(HS_FLAGS) $(LIBS) -ghci-script test/test.ghci <test/test.R


build/c/embedR-main: src/c/embedR.c
	@mkdir -p build/c
	gcc $(C_FLAGS) $(LIBS) -o $@ $<

build/c/embedR-aux: src/c/embedR.c
	@mkdir -p build/c
	gcc $(C_FLAGS) $(LIBS) -DAUX -o $@ $<

build/hs/embedR-main: src/hs/embedR.hs
	@mkdir -p build/hs
	ghc -threaded $(HS_FLAGS) $(LIBS) -o $@ $<

build/hs/embedR-aux: src/hs/embedR.hs
	@mkdir -p build/hs
	ghc -threaded $(HS_FLAGS) $(LIBS) -DAUX -o $@ $<


clean:
	rm -rf build
