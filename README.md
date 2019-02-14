-------------------------------------------------------------------------------

This project is no longer maintained.

-------------------------------------------------------------------------------


_embed-r_
=========

Example C and Haskell programs, showing how to mesh with the R event loop.

Special attention is given ensuring the R interactive interpreter correctly handles GUI events while embedded in the Haskell interactive interpreter, GHCi.


Usage
-----

Install R and _pkg-config_.


### Interactive testing

Use `make run-c`, `make run-ghc`, and `make run-ghci` to run each test individually.


#### C

```
$ make run-c
...
-----> Embedding R in C
R_HOME=/usr/local/Cellar/r/3.1.2/R.framework/Versions/3.1/Resources build/embedR-c
-----> C: Starting R
-----> C: Found exported R toplevel context
plot(cars)
-----> C: Parsing 'plot(cars)'
print("fnord")
-----> C: Parsing 'print("fnord")'
[1] "fnord"
q()
-----> C: Parsing 'q()'
```


#### Haskell

```
$ make run-ghc
...
-----> Embedding R in Haskell
R_HOME=/usr/local/Cellar/r/3.1.2/R.framework/Versions/3.1/Resources build/embedR-ghc
-----> Haskell: Starting R
-----> C: Found exported R toplevel context
plot(cars)
-----> C: Parsing 'plot(cars)'
print("fnord")
-----> C: Parsing 'print("fnord")'
[1] "fnord"
q()
-----> C: Parsing 'q()'
```


#### Haskell using GHCi

```
$ make run-ghci
...
-----> Embedding R in Haskell using GHCi
R_HOME=/usr/local/Cellar/r/3.1.2/R.framework/Versions/3.1/Resources ghci -Wall -fno-ghci-sandbox -hidir build -odir build build/altR.dylib
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
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
```


### Automatic testing

Use `make test` to run all tests, or `make test-c`, `make test-ghc`, and `make test-ghci` to run each test individually.


#### C

```
$ make test-c
...
-----> Embedding R in C
R_HOME=/usr/local/Cellar/r/3.1.2/R.framework/Versions/3.1/Resources build/embedR-c <test/test.R
-----> C: Starting R
-----> C: Found exported R toplevel context
-----> C: Parsing 'plot(cars)'
-----> C: Parsing 'print("fnord")'
[1] "fnord"
-----> C: Parsing 'Sys.sleep(1)'
-----> C: Exiting R
```


#### Haskell

```
$ make test-ghc
...
-----> Embedding R in Haskell
R_HOME=/usr/local/Cellar/r/3.1.2/R.framework/Versions/3.1/Resources build/embedR-ghc <test/test.R
-----> Haskell: Starting R
-----> C: Found exported R toplevel context
-----> C: Parsing 'plot(cars)'
-----> C: Parsing 'print("fnord")'
[1] "fnord"
-----> C: Parsing 'Sys.sleep(1)'
-----> Haskell: Exiting R
```


#### Haskell using GHCi

```
$ make test-ghci
...
-----> Embedding R in Haskell using GHCi
R_HOME=/usr/local/Cellar/r/3.1.2/R.framework/Versions/3.1/Resources ghci -Wall -fno-ghci-sandbox -hidir build -odir build build/altR.dylib -ghci-script test/test.ghci <test/test.R
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
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
```


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the BSD license.
