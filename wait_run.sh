#!/bin/bash

STARTED=false

while ! $STARTED; do
	NR=$(ps -u adam -o comm= | grep '^R$' | wc -l)
	echo $NR
	if [ $NR -eq 0 ]; then
		echo 'data/sep_nx5_local/' > run_scripts/data_dir.txt
		echo 'data/separate_data_tools.R' > run_scripts/data_tools_file.txt
		sh ./run_scripts/run_sim_and_tpr_fpr_and_AUC.sh -p 4
		STARTED=true
	fi
	sleep 30s
done
