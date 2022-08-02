#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

# assumes cabal 2.4 or later
cabal v2-sdist
cabal upload --publish dist-newstyle/sdist/*.tar.gz

cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d --publish $dir/*-docs.tar.gz
