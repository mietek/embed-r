----


EmbedR
======

Experiments in embedding R.

Run `make` to quickly test embedding R in C and Haskell.


Using the main thread
---------------------

This works.

On OS X, in C:

    $ make clean && make test-c
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -lR  -Wall -o build/embedR-c -std=c99 build/altR.o src/embedR.c
    -----> Embedding R in C...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-c <test/test.R
    -----> C: Starting R...
    -----> C: Parsing 'plot(cars)'...
    -----> C: Parsing 'Sys.sleep(1)'...
    -----> C: Exiting R...

On OS X, in Haskell:

    $ make clean && make test-hs
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    ghc -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -lR  -Wall -hidir build -odir build -o build/embedR-hs -threaded build/altR.o src/embedR.hs
    [1 of 1] Compiling Main             ( src/embedR.hs, build/Main.o )
    Linking build/embedR-hs ...
    -----> Embedding R in Haskell...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-hs <test/test.R
    -----> Haskell: Starting R...
    -----> C: Parsing 'plot(cars)'...
    -----> C: Parsing 'Sys.sleep(1)'...
    -----> Haskell: Exiting R...

On OS X, in Haskell, using GHCi with `-fno-ghci-sandbox`:

    $ make clean && make test-hs-i
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    gcc -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -lR  -shared -o build/altR.dylib -std=c99 build/altR.o
    -----> Embedding R in Haskell using GHCi...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci -Wall -fno-ghci-sandbox -hidir build -odir build build/altR.dylib -ghci-script test/test.ghci <test/test.R
    GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading object (dynamic) build/altR.dylib ... done
    final link ... done
    -----> GHCi: Starting GHCi script...
    [1 of 1] Compiling Main             ( src/embedR.hs, interpreted )
    Ok, modules loaded: Main.
    -----> Haskell: Starting R...
    -----> C: Parsing 'plot(cars)'...
    -----> C: Parsing 'Sys.sleep(1)'...
    -----> Haskell: Exiting R...
    -----> GHCi: Exiting GHCi script...
    > Leaving GHCi.


Using an auxiliary thread
-------------------------

This does not work and is not expected to work.

On OS X, in C, using `pthread_create()`:

    $ make clean && make test-c-aux
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -lR  -DAUX -Wall -o build/embedR-c-aux -std=c99 build/altR.o src/embedR.c
    -----> Embedding R in C using an auxiliary thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-c-aux <test/test.R
    -----> C: Starting R...
    Error: C stack usage  140730371871004 is too close to the limit
    Error: C stack usage  140730371871052 is too close to the limit
    -----> C: Parsing 'plot(cars)'...
    Error: C stack usage  140730371867132 is too close to the limit
    /bin/sh: line 1: 88282 Abort trap: 6           R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-c-aux < test/test.R
    make: *** [test-c-aux] Error 134

On OS X, in Haskell, using `forkOS`:

    $ make clean && make test-hs-aux
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    ghc -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -lR  -DAUX -Wall -hidir build -odir build -o build/embedR-hs-aux -threaded build/altR.o src/embedR.hs
    [1 of 1] Compiling Main             ( src/embedR.hs, build/Main.o )
    Linking build/embedR-hs-aux ...
    -----> Embedding R in Haskell using an auxiliary thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-hs-aux <test/test.R
    -----> Haskell: Starting R...
    Error: C stack usage  140730086932972 is too close to the limit
    Error: C stack usage  140730086933020 is too close to the limit
    -----> C: Parsing 'plot(cars)'...
    Error: C stack usage  140730086929100 is too close to the limit
    /bin/sh: line 1: 88389 Abort trap: 6           R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/embedR-hs-aux < test/test.R
    make: *** [test-hs-aux] Error 134

On OS X, in Haskell, using GHCi without `-fno-ghci-sandbox` — requires `killall -KILL ghc`:

    $ make clean && make test-hs-i-aux
    rm -rf build
    gcc -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -Wall -c -fPIC -o build/altR.o -std=c99 src/altR.c
    gcc -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib  -lR  -shared -o build/altR.dylib -std=c99 build/altR.o
    -----> Embedding R in Haskell using an auxilliary thread within GHCi...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci -Wall -hidir build -odir build build/altR.dylib -ghci-script test/test.ghci <test/test.R
    GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading object (dynamic) build/altR.dylib ... done
    final link ... done
    -----> GHCi: Starting GHCi script...
    [1 of 1] Compiling Main             ( src/embedR.hs, interpreted )
    Ok, modules loaded: Main.
    -----> Haskell: Starting R...
    Error: C stack usage  140729880404780 is too close to the limit
    Error: C stack usage  140729880404828 is too close to the limit
    -----> C: Parsing 'plot(cars)'...
    Error: C stack usage  140729880400876 is too close to the limit

     *** caught segfault ***
    address 0x114168c5, cause 'memory not mapped'

    Possible actions:
    1: abort (with core dump, if enabled)
    2: normal R exit
    3: exit R without saving workspace
    4: exit R saving workspace
    Selection: Error: could not find function "try"

     *** caught segfault ***
    address 0x114168c5, cause 'memory not mapped'

    Possible actions:
    1: abort (with core dump, if enabled)
    2: normal R exit
    3: exit R without saving workspace
    4: exit R saving workspace
    Selection: Error: could not find function "try"
    /bin/sh: line 1: 88601 Killed: 9               R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci -Wall -hidir build -odir build build/altR.dylib -ghci-script test/test.ghci < test/test.R
    make: *** [test-hs-i-aux] Error 137


Meta
----

Written by [Miëtek Bak][].  Say hello@mietek.io

Available under the MIT License.


----

[Miëtek Bak]: http://mietek.io
