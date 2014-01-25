#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

pushd "$cwd/.."
.cabal-sandbox/bin/fay --include --include=../.cabal-sandbox --package fay-text,fay-dom "$cwd/mapview.hs"
echo "Using Google's Closure Compiler with 'advanced optimizations'"
echo "This might be slow on some computers..."
closure-compiler --externs "$cwd/google_maps_api_v3_11.js" --compilation_level ADVANCED_OPTIMIZATIONS "$cwd/mapview.js" > "$cwd/mapview.min.js" 2>/dev/null
rm "$cwd/mapview.js"
popd
