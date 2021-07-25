#!/bin/bash

# Easy to forget to pass argument!
if [ $# = 0 ]; then
    echo 'Remember to pass an argument.'
    echo 'Run ./run_scripts/run_sim_and_tpr_fpr_and_AUC.sh -h for help.'
    exit 1
fi

# Runs the main scripts
nohup sh ./run_scripts/run_sim_and_tpr_fpr_and_AUC.sh "$@" > /dev/null &

# Saves PID of the script
echo $! > run_scripts/.output/sh_PID.txt