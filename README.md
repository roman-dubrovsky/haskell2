# Lab2

#### Build

```bash
cabal install --only-dependencies
cabal build
```

#### Call example
```
$ ./dist/build/lab2/lab2
Iris-setosa - 1(5.1727;8.3982) - 2(3.5636;0.1557) - 3(1.4954;3.0930) - 4(0.2545;1.3073)
Iris-versicolor - 1(5.9499;0.2289) - 2(2.7769;9.3046) - 3(4.2730;0.2004) - 4(1.3346;4.7153)
Iris-virginica - 1(6.6450;0.6647) - 2(2.96;0.1688) - 3(5.6149;0.4529) - 4(1.97;7.6947)
```

#### Help
```
Lab2 Bayes 2015

lab2 [OPTIONS]

Common flags:
  -d --delemiter=ITEM   Csv delemiter
  -i --inputfile=ITEM   Input file name
  -o --outputfile=ITEM  Output file name (default console)
  -h --header           Have csv header?
  -r --rownumber        Have csv number (row's head)?
  -n --number=INT       Numbers count for studing
  -p --percent=INT      Percent on learning objects
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```

## Lab3

```
ghc -threaded -eventlog -rtsopts --make Main.hs
./Main +RTS -ls -N2
```
