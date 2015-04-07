boot:: sandbox
	cabal install --only-dependencies --enable-tests
	cabal build

sandbox::
	cabal sandbox init

clean:
	cabal sandbox delete
	cabal clean
