#!/bin/sh

sha=$(command git log -n 1 --pretty=format:"%H")
branch=$(git rev-parse --abbrev-ref HEAD)

tag="${branch}_${sha}"

echo "TAG=$tag" > .env

if [ $# -ge 1 ] && [ -n "$1" ]
then 
  composer=$1

  if [ $composer == "yes" ] ; then
   exec docker-compose up
  fi
fi