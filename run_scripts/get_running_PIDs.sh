#!/bin/bash

## Gives PIDs of currently running simulations and processing

ps -e -o pid= | grep "$(cut -d " " -f1 run_scripts/.output/run.txt)"
