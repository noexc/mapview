installdeps:
	@echo "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
	@echo "> NOTE! This Makefile is temporary until some patches get released upstream <"
	@echo "> Don't script against it or get used to it.                                <"
	@echo ">^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^<"
	cabal sandbox init
	git submodule update --init
	cabal install gitdeps/*
	cabal install --only-dependencies
	cabal install --enable-tests
	@echo "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
	@echo "> NOTE! This Makefile is temporary until some patches get merged upstream <"
	@echo "> Don't script against it or get used to it.                              <"
	@echo ">^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^<"
