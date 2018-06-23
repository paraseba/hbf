# HBF

A Brainfuck optimizing compiler and evaluator.

[![pipeline status](https://gitlab.com/paraseba/hbf/badges/master/pipeline.svg)](https://gitlab.com/paraseba/hbf/commits/master)

## Getting Started

Install HBF from Hackage using cabal or stack.

```
cabal build
```

Now to compile a Brainfuck program:

```
./dist/build/hbfc/hbfc sourceFile.bf
```

This will create the compiled program in sourceFile.bf. For more options run

```
./dist/build/hbfc/hbfc --help
```

Once you have the compiled program you can run it taking input and output from stdin/stdout:

```
./dist/build/hbf/hbf sourceFile.bfc
```

or use `--help` for more options.

## Running the tests

```
cabal test
cabal doctest
```

Test coverage report for the last build can be found [here](https://paraseba.gitlab.io/hbf/coverage/hpc_index.html).

## Running the benchmarks

```
cabal bench
```

Benchmark report for the last build can be found [here](https://paraseba.gitlab.io/hbf/bench.htm).

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details

Copyright 2018 Sebastian Galkin.

## Acknowledgments

This work is based on the excellent project [bfoptimization](https://github.com/matslina/bfoptimization) by matslina: 

