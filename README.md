----


EmbedR
======

Experiments in embedding R.


Manual testing
--------------

Use `make run-c`, `make run-ghc`, and `make run-ghci` to run each test individually.

### Embedding R in C

    $ make run-c
    -----> Embedding R in C
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-c
    -----> C: Starting R
    -----> C: Found exported R toplevel context
    plot(cars)
    -----> C: Parsing 'plot(cars)'
    print("fnord")
    -----> C: Parsing 'print("fnord")'
    [1] "fnord"
    q()
    -----> C: Parsing 'q()'

### Embedding R in Haskell

    $ make run-ghc
    -----> Embedding R in Haskell
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-ghc
    -----> Haskell: Starting R
    -----> C: Found exported R toplevel context
    plot(cars)
    -----> C: Parsing 'plot(cars)'
    print("fnord")
    -----> C: Parsing 'print("fnord")'
    [1] "fnord"
    q()
    -----> C: Parsing 'q()'

### Embedding R in Haskell using GHCi

    $ make run-ghci
    -----> Embedding R in Haskell using GHCi
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci -Wall -fno-ghci-sandbox -hidir build -odir build build/altR.dylib
    GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading object (dynamic) build/altR.dylib ... done
    final link ... done
    > :load src/embedR.hs
    [1 of 1] Compiling Main             ( src/embedR.hs, interpreted )
    Ok, modules loaded: Main.
    > initR
    -----> C: Found exported R toplevel context
    > do1LineR
    plot(cars)
    -----> C: Parsing 'plot(cars)'
    True
    > putStrLn "fnord"
    fnord
    > do1LineR
    print("fnord")
    -----> C: Parsing 'print("fnord")'
    [1] "fnord"
    True
    > endR
    > :q
    Leaving GHCi.


Automatic testing
-----------------

Use `make test` to run all tests, or `make test-c`, `make test-ghc`, and `make test-ghci` to run each test individually.

### Embedding R in C

    $ make clean && make test-c
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -Wall -o build/embedR-c -std=c99 build/altR.o src/embedR.c -lR  -lpthread
    -----> Embedding R in C
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-c <test/test.R
    -----> C: Starting R
    -----> C: Found exported R toplevel context
    -----> C: Parsing 'plot(cars)'
    -----> C: Parsing 'print("fnord")'
    [1] "fnord"
    -----> C: Parsing 'Sys.sleep(1)'
    -----> C: Exiting R

### Embedding R in Haskell

    $ make clean && make test-ghc
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    ghc -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -Wall -hidir build -odir build -o build/embedR-ghc -threaded build/altR.o src/embedR.hs -lR  -lpthread
    [1 of 1] Compiling Main             ( src/embedR.hs, build/Main.o )
    Linking build/embedR-ghc ...
    -----> Embedding R in Haskell
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-ghc <test/test.R
    -----> Haskell: Starting R
    -----> C: Found exported R toplevel context
    -----> C: Parsing 'plot(cars)'
    -----> C: Parsing 'print("fnord")'
    [1] "fnord"
    -----> C: Parsing 'Sys.sleep(1)'
    -----> Haskell: Exiting R

### Embedding R in Haskell using GHCi

    $ make clean && make test-ghci
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    gcc -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -shared -o build/altR.dylib -std=c99 build/altR.o -lR
    -----> Embedding R in Haskell using GHCi
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci -Wall -fno-ghci-sandbox -hidir build -odir build build/altR.dylib -ghci-script test/test.ghci <test/test.R
    GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading object (dynamic) build/altR.dylib ... done
    final link ... done
    -----> GHCi: Starting GHCi script
    [1 of 1] Compiling Main             ( src/embedR.hs, interpreted )
    Ok, modules loaded: Main.
    -----> Haskell: Starting R
    -----> C: Found exported R toplevel context
    -----> C: Parsing 'plot(cars)'
    -----> C: Parsing 'print("fnord")'
    [1] "fnord"
    -----> C: Parsing 'Sys.sleep(1)'
    -----> Haskell: Exiting R
    -----> GHCi: Exiting GHCi script
    > Leaving GHCi.


Meta
----

Written by [Miëtek Bak][].  Say hello@mietek.io

Available under the MIT License.


----

[Miëtek Bak]: http://mietek.io
