#!/bin/bash

if [ $# = 0 ]; then
	DIR="data"
else
	DIR=$1
fi

ls $DIR | grep "^singletargets" | sed 's/singletargets_\([0-9]\+\)_\([0-9]\+\)_\([0-9]\+\)_\([0-9]\+\)_sdw\([0-9]\+\)_sdh\([0-9]\+\)/\1 \2 \3 \4 \5 \6/' > "singletargets_params_${DIR}.txt"

ls $DIR | grep "^alltargets" | sed 's/alltargets_\([0-9]\+\)_\([0-9]\+\)_sdw\([0-9]\+\)_sdh\([0-9]\+\)/\1 \2 \3 \4/' > "alltargets_params_${DIR}.txt"
