#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone git@github.com:noexc/mapview.git "$f/mapview.git"
cabal haddock
pushd "$f/mapview.git"
  git checkout gh-pages && git rm -rf *
  mkdir haddock
popd
mv dist/doc/html/mapview/* "$f/mapview.git/haddock"
pushd "$f/mapview.git"
  git checkout master -- doc
  pushd doc
    asciidoctor *.adoc
    cp Introduction.html index.html
    rm *.adoc
  popd
  mv doc/*.html .
  rmdir doc
  echo 'mapview.noexc.org' > CNAME
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://noexc.github.io/mapview/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
