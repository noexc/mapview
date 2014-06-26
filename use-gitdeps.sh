#/usr/bin/env bash
echo "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
echo "> NOTE! This script is temporary until some patches get released upstream <"
echo "> Don't script against it or get used to it.                              <"
echo ">^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^<"
cabal sandbox init
git submodule update --init
cabal install gitdeps/* mapview.cabal 'transformers >= 0.4'
cabal install --enable-tests mapview.cabal
echo "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
echo "> NOTE! This script is temporary until some patches get merged upstream <"
echo "> Don't script against it or get used to it.                            <"
echo ">^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^<"
