----


EmbedR
======

Experiments in embedding R.

Run `make` to quickly test embedding R in C and Haskell, both within a compiled program and within GHCi.


Using the main thread
---------------------

This works properly.

In C:

    $ make test-c-main
    gcc -Wall -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o build/c/embedR-main src/c/embedR.c
    -----> Embedding R in C using the main thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/c/embedR-main <test/test.R
    ----C> Starting R...
    > #-R> Starting R script...
    > plot(cars)
    > Sys.sleep(1)
    > #-R> Exiting R script...
    >
    ----C> Exiting R...

In Haskell, within a compiled program:

    $ make test-hs-main
    ghc -threaded -Wall -hidir build/hs -odir build/hs -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o build/hs/embedR-main src/hs/embedR.hs
    [1 of 1] Compiling Main             ( src/hs/embedR.hs, build/hs/Main.o )
    Linking build/hs/embedR-main ...
    -----> Embedding R in Haskell using the main thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/hs/embedR-main <test/test.R
    ----H> Starting R...
    > #-R> Starting R script...
    > plot(cars)
    > Sys.sleep(1)
    > #-R> Exiting R script...
    >
    ----H> Exiting R...

In Haskell, within GHCi, using `-fno-ghci-sandbox`:

    $ make test-hs-main-i
    -----> Embedding R in Haskell using the main thread within GHCi...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci -fno-ghci-sandbox src/hs/embedR.hs -Wall -hidir build/hs -odir build/hs -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -ghci-script test/test.ghci <test/test.R
    GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading object (dynamic) /usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib/libR.dylib ... done
    final link ... done
    -GHCi> Starting GHCi script...
    [1 of 1] Compiling Main             ( src/hs/embedR.hs, interpreted )
    Ok, modules loaded: Main.
    ----H> Starting R...
    > #-R> Starting R script...
    > plot(cars)
    > Sys.sleep(1)
    > #-R> Exiting R script...
    >
    ----H> Exiting R...
    -GHCi> Exiting GHCi script...
    Loading package array-0.5.0.0 ... linking ... done.
    Loading package deepseq-1.3.0.2 ... linking ... done.
    Loading package bytestring-0.10.4.0 ... linking ... done.
    Loading package old-locale-1.0.0.6 ... linking ... done.
    Loading package time-1.4.2 ... linking ... done.
    Loading package unix-2.7.0.1 ... linking ... done.


Using an auxiliary thread
-------------------------

This does not work properly.

In C, using `pthread_create()`:

    $ make test-c-aux
    gcc -Wall -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include  -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -DAUX -o build/c/embedR-aux src/c/embedR.c
    -----> Embedding R in C using an auxiliary thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/c/embedR-aux <test/test.R
    ----C> Starting R...
    Error: C stack usage  140730192703772 is too close to the limit
    Error: C stack usage  140730192703820 is too close to the limit
    ...

In Haskell, using `forkOS`:

    $ make test-hs-aux
    ghc -threaded -Wall -hidir build/hs -odir build/hs -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -DAUX -o build/hs/embedR-aux src/hs/embedR.hs
    [1 of 1] Compiling Main             ( src/hs/embedR.hs, build/hs/Main.o )
    Linking build/hs/embedR-aux ...
    -----> Embedding R in Haskell using an auxiliary thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/hs/embedR-aux <test/test.R
    ----H> Starting R...
    Error: C stack usage  140730316308972 is too close to the limit
    Error: C stack usage  140730316309020 is too close to the limit
    ...

In Haskell, within GHCi, not using `-fno-ghci-sandbox`:

    $ make test-hs-aux-i
    -----> Embedding R in Haskell using an auxilliary thread within GHCi...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ghci src/hs/embedR.hs -Wall -hidir build/hs -odir build/hs -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -ghci-script test/test.ghci <test/test.R
    GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading object (dynamic) /usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib/libR.dylib ... done
    final link ... done
    -GHCi> Starting GHCi script...
    [1 of 1] Compiling Main             ( src/hs/embedR.hs, interpreted )
    Ok, modules loaded: Main.
    ----H> Starting R...
    Error: C stack usage  140730031457068 is too close to the limit
    Error: C stack usage  140730031457116 is too close to the limit



Meta
----

Written by [Miëtek Bak][].  Say hello@mietek.io

Available under the MIT License.


----

[Miëtek Bak]: http://mietek.io
