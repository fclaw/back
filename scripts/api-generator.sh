#!/bin/bash

file=$1
dir=$2
$nm
$package

openapi3-code-generator-exe -f -o $dir/src/foreign/$mn \
  --package-name $package --property-type-suffix github --generate-optional-empty-request-body=false $file && \
  python $dir/scripts/duplicate.py $dir/src/foreign/$nm/$package.cabal
