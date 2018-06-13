shell = @nix-shell --attr hbf.env release.nix

watchtest:
	$(shell) --run 'ghcid -W -T Main.main'

watchbuild:
	$(shell) --run 'ghcid'

style:
	find -name '*.hs' -exec hindent  {} \;
	find -name '*.hs' -exec stylish-haskell -i {} \;

clean:
	cabal clean

configure: default.nix release.nix hbf.cabal
	$(shell) --run 'cabal configure --enable-tests --enable-benchmarks'

build:
	cabal build

test: ./dist/build/test/test
	./dist/build/test/test

./dist/build/test/test: tests/* src/*
	cabal build test

bench: ./dist/build/evalbench/evalbench
	@mkdir -p public/
	./dist/build/evalbench/evalbench -o public/bench.html

./dist/build/evalbench/evalbench: bench/BenchEval.hs src/*
	cabal build evalbench

default.nix: hbf.cabal
	@cabal2nix . > default.nix

./dist/build/hbf/hbf: src/* exe/Hbf.hs
	cabal build exe:hbf

./dist/build/hbfc/hbfc: src/* exe/Hbfc.hs
	cabal build exe:hbfc

vm: ./dist/build/hbf/hbf
compiler: ./dist/build/hbfc/hbfc

coverage:
	make clean
	$(shell) --run 'cabal configure --enable-tests --enable-coverage'
	cabal test
	@mkdir -p public/coverage
	@cp -r dist/hpc/vanilla/html/test public/coverage
	make clean
	make configure

.PHONY : bench test configure watchbuild watchtest clean vm compiler build style coverage
