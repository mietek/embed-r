----


EmbedR
======

Experiments in embedding R.


Using the main thread
---------------------

This works properly.

In C:

    $ make test-c-main
    gcc -Wall -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o build/c/embedR-main src/c/embedR.c
    -----> Embedding R in C using the main thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/c/embedR-main <test/test.R
    > print("Hello, embedR!")
    [1] "Hello, embedR!"
    > plot(cars)
    > Sys.sleep(1)

In Haskell:

    $ make test-hs-main
    ghc -Wall -hidir build/hs -odir build/hs -threaded -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o build/hs/embedR-main src/hs/embedR.hs
    [1 of 1] Compiling Main             ( src/hs/embedR.hs, build/hs/Main.o )
    Linking build/hs/embedR-main ...
    -----> Embedding R in Haskell using the main thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/hs/embedR-main <test/test.R
    > print("Hello, embedR!")
    [1] "Hello, embedR!"
    > plot(cars)
    > Sys.sleep(1)


Using an auxiliary thread
-------------------------

This is not expected to work properly.

In C, using `pthread`:

    $ make test-c-aux
    gcc -DAUX -Wall -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o build/c/embedR-aux src/c/embedR.c
    -----> Embedding R in C using an auxiliary pthread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/c/embedR-aux <test/test.R
    Error: C stack usage  140730081747228 is too close to the limit
    Error: C stack usage  140730081747276 is too close to the limit
    ...

In Haskell, using `forkOS`:

    $ make test-hs-aux
    ghc -DAUX -Wall -hidir build/hs -odir build/hs -threaded -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o build/hs/embedR-aux src/hs/embedR.hs
    [1 of 1] Compiling Main             ( src/hs/embedR.hs, build/hs/Main.o )
    Linking build/hs/embedR-aux ...
    -----> Embedding R in Haskell using an auxiliary OS thread...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources build/hs/embedR-aux <test/test.R
    Error: C stack usage  140730375025132 is too close to the limit
    Error: C stack usage  140730375025180 is too close to the limit
    ...


Meta
----

Written by [Miëtek Bak][].  Say hello@mietek.io

Available under the MIT License.


----

[Miëtek Bak]: http://mietek.io
