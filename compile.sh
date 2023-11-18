#!/bin/bash

dir=./solutions/$1/day$2

ghc $dir/Part$3.hs -no-keep-hi-files -no-keep-o-files -i$dir
