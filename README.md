# HBF

A Brainfuck optimizing compiler and evaluator.

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

## Running the benchmarks

```
cabal bench
```



## Authors

* **Sebastian Galkin**

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

This work is based on the excellent project [bfoptimization](https://github.com/matslina/bfoptimization) by matslina: 

