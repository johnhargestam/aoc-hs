#!/bin/bash

if [[ $1 -ge 2015 && $1 -le $(date +%Y) ]]; then
  year=$1
else
  echo "missing valid argument for year"
  exit 1
fi
if [[ $2 -ge 1 && $2 -le 25 ]]; then
  day=$2
else
  echo "missing valid argument for day"
  exit 1
fi
if [[ $3 -ge 1 && $3 -le 2 ]]; then
  part=$3
else
  echo "missing valid argument for part"
  exit 1
fi

source .env
dir=./solutions/$year/day$day

if [ ! -d $dir ]; then
  mkdir -p $dir
  cp -r ./template/* $dir
  sed -i -e "s/template/solutions\\/$year\\/day$day/g" $dir/Day.hs
  curl https://adventofcode.com/$year/day/$day/input \
       --cookie "session=$AOC_SESSION" \
       -o "$dir/input"
fi

ghci $dir/Part$part.hs -i$dir
