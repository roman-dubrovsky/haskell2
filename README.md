# Lab2

#### Build

```bash
cabal install --only-dependencies
cabal build
```

#### Call example
```bash
./dist/build/lab2/lab2
```

#### Help
```bash
Lab2 Bayes 2015

lab2 [OPTIONS]

Common flags:
  -d --delemiter=ITEM   Csv delemiter
  -i --inputfile=ITEM   Input file name
  -o --outputfile=ITEM  Output file name (default console)
  -h --header           Have csv header?
  -r --rownumber        Have csv number (row's head)?
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```
