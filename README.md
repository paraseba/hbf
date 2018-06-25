# HBF [![Build Status](https://travis-ci.org/paraseba/hbf.svg?branch=master)](https://travis-ci.org/paraseba/hbf) [![Hackage](
https://img.shields.io/hackage/v/hbf.svg)](https://hackage.haskell.org/package/hbf)



An unoptimized Brainfuck optimizing compiler and evaluator.

[HBF](https://github.com/paraseba/hbf) can compile standard Brainfuck code into an optimized
Intermediate Representation (IR). This IR can later be executed for faster Brainfuck program
evaluation.


## Getting Started

### Installing
Use your preferred method for installing HBF from [Hackage](fixme-link). If you prefer to build from source
you can use cabal.

### Usage

Let's say you have a Brainfuck program in the file `mandelbrot.bf`, for example,
you can get one [here](https://github.com/pablojorge/brainfuck/blob/master/programs/mandelbrot.bf)

You can compile the program to the Intermediate Representation using the `hbfc` executable provided by this project:

```
hbfc mandelbrot.bf
```

That will create a file `mandelbrot.bfc` in the same directory. If you want the output in a different
path you cane use `--output` option to `hbfc`

Now, you can run the compiled program:

```
hbf mandelbrot.bfc
```

and get something like:

![Mandelbrot](https://raw.githubusercontent.com/paraseba/hbf/e86d1ffebcb0795a7c2c6081e2dd27c4154db066/mandelbrot.png)


There are several options  to both the compiler and the evaluator, to modify levels of optimization, output and others. Try
`--help` to learn more.


## Hacking
HBF has heavily documented code. Take a look at the documentation on [Hackage](fixme-link).


### Running the tests

```
cabal test
```

Test coverage report for the last build can be found [here](https://paraseba.gitlab.io/hbf/coverage/hpc_index.html).

### Running the benchmarks

```
cabal bench
```

Benchmark report for the last build can be found [here](https://paraseba.gitlab.io/hbf/bench.html).

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details

Copyright 2018 Sebastian Galkin.

## Acknowledgments

This work is based on the excellent project [bfoptimization](https://github.com/matslina/bfoptimization) by matslina: 

