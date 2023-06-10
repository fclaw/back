#!/bin/bash

file=$1
dir=$2

openapi3-code-generator-exe -f -o $dir/src/foreign/Sendgrid \
  --package-name sendgrid --property-type-suffix sendgrid $file && \
  python $dir/scripts/duplicate.py $dir/src/foreign/Sendgrid/sendgrid.cabal
