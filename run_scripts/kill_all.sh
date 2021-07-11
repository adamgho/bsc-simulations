#!/bin/bash

cat run_scripts/.output/sh_PID.txt | xargs kill
sh ./run_scripts/get_running_PIDs.sh | xargs kill
