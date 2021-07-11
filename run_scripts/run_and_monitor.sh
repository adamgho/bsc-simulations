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

# Monitors running R scripts
sh ./run_scripts/cont_list_running.sh

# Kills all processes (useful when user quits monitoring)
read -p 'Kill processes (y/n)?: ' TOKILL
if [ $TOKILL = y ]; then
    sh ./run_scripts/kill_all.sh
    echo 'All processes killed.'
else
    echo "Ok. I won't kill the processes."
fi
