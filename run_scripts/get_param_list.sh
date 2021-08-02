#!/bin/bash

## Saves list of parameters used for simulations.

## Give data directory as command line argument.
## If no argument is given, the directory is assumed to be named 'data'.
if [ $# = 0 ]; then
	DIR="data"
else
	DIR=$1
fi

## Saves in txt files in current dir.

ls $DIR | grep "^singletargets" | sed 's/singletargets_\([0-9]\+\)_\([0-9]\+\)_\([0-9]\+\)_\([0-9]\+\)_sdw\([0-9]\+\)_sdh\([0-9]\+\)/\1 \2 \3 \4 \5 \6/' > "singletargets_params_${DIR}.txt"

ls $DIR | grep "^alltargets" | sed 's/alltargets_\([0-9]\+\)_\([0-9]\+\)_sdw\([0-9]\+\)_sdh\([0-9]\+\)/\1 \2 \3 \4/' > "alltargets_params_${DIR}.txt"
