#!/bin/bash

## Lists currently running simulations and processing with useful info

join <(ps -e -o pid,time,pcpu,pmem | grep "$(cut -d " " -f1 run_scripts/.output/run.txt)\|PID" | sort) <(grep "$(pgrep R)" run_scripts/.output/run.txt | sort) | column -t -l 7 -N PID,TIME,%CPU,%MEM,WHAT,SIMTYPE,PARAMETERS 
