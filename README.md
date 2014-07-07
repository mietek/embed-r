----


EmbedR
======

Experiments in embedding R.

    $ make
    gcc -Wall -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o cembedR cembedR.c
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ./cembedR < R.cmd
    > print("Hello, embedR!")
    [1] "Hello, embedR!"
    > plot(cars)
    > Sys.sleep(1)
    > ghc -Wall -threaded -I/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/include -L/usr/local/lib -L/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources/lib -lR  -o hsembedR hsembedR.hs
    [1 of 1] Compiling Main             ( hsembedR.hs, hsembedR.o )
    Linking hsembedR ...
    R_HOME=/usr/local/Cellar/r/3.1.0/R.framework/Versions/3.1/Resources ./hsembedR < R.cmd
    > print("Hello, embedR!")
    [1] "Hello, embedR!"
    > plot(cars)
    > Sys.sleep(1)


Meta
----

Written by [Miëtek Bak][].  Say hello@mietek.io

Available under the MIT License.


----

[Miëtek Bak]: http://mietek.io
