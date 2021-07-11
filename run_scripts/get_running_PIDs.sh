#!/bin/bash

ps -e -o pid= | grep "$(cut -d " " -f1 run_scripts/.output/run.txt)"
