#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

pushd "$cwd/.."
export HASKELL_PACKAGE_SANDBOX="$(echo .cabal-sandbox/*-packages.conf.d)"
export PATH=".cabal-sandbox/bin:$PATH"
.cabal-sandbox/bin/fay --include --include=../.cabal-sandbox --package fay-text,fay-dom "$cwd/mapview.hs"
if [ "$1" == "dev" ]; then
  echo "[dev] Not sending to Closure Compiler to minify."
  mv "$cwd/mapview.js" "$cwd/mapview.min.js"
else
  echo "Using Google's Closure Compiler with 'advanced optimizations'"
  echo "This might be slow on some computers..."
  closure-compiler --externs /usr/share/closure-compiler/externs/contrib/maps/google_maps_api_v3_15.js --compilation_level ADVANCED_OPTIMIZATIONS "$cwd/mapview.js" > "$cwd/mapview.min.js" 2>/dev/null
  rm "$cwd/mapview.js"
fi
popd
