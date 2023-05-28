#!/bin/sh

sha=$(command git log -n 1 --pretty=format:"%H")
branch=$(git rev-parse --abbrev-ref HEAD)

tag="${branch}_${sha}"

echo "DBUSER=sonny\nDATABASE=scaffold\nTAG=$tag" > .env