#!/bin/bash
# This little script generates the packages shipped with the compiler from
# the file provided here: https://gitlab.haskell.org/bgamari/ghc-utils/blob/master/library-versions/pkg_versions.txt
#
# Usage: ./gen-compiler-packages.sh pkg_versions.txt > compiler-packages.yaml
#
# The general idea is to collect the versions in the file first, then
# during a secon pass collect the relevant library lines, and finally
# format them as yaml.
#
# We'll skip the HEAD version as that will mostly be in flux.

VERSIONS=$(grep -h -v -E "^#|^$" "$1" | awk -F\  '{ print $1 }'| sort | uniq)
for version in $VERSIONS; do
    if [[ $version != "HEAD" ]]; then
        echo "ghc-${version}:"
        LIBRARIES=$(grep -h "^${version}" "$1" | sort)
        for lib in $LIBRARIES; do
            if [[ "$lib" != "$version" ]]; then
                # packages with "*" suffix are hidden; we don't care about
                # hidden here.
                echo "$lib" | awk -F/ '{ print "  "$1": \""$2"\"" }' | sed 's/*"/"/'
            fi
        done
    fi
done