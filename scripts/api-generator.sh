#!/bin/bash

file=$1
nm=$2
package=$3
dir=$4

openapi3-code-generator-exe \
  -f -o $dir/src/foreign/$mn \
  --package-name $package \
  --property-type-suffix github \
  --generate-optional-empty-request-body=false $file && \
  python $dir/scripts/duplicate.py $dir/src/foreign/$nm/$package.cabal
