#!/bin/bash

source .env
dir=./solutions/$1/day$2

if [ ! -d $dir ]; then
  mkdir -p $dir
  cp -r ./template/* $dir
  sed -i -e "s/template/solutions\\/$1\\/day$2/g" $dir/Day.hs
  curl https://adventofcode.com/$1/day/$2/input \
       --cookie "session=$AOC_SESSION" \
       -o "$dir/input"
fi

ghci $dir/Part$3.hs -i$dir
