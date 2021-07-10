#!/bin/bash

# This script simulates and processes all data setups given in the files
# alltargets_params.txt and singletargets_params.txt.
# Each line in the files should contain the parameters for the given
# simulation setup, in the order in which they are passed to the
# sim_alltargets respectively sim_singletargets functions.

if [ $# = 0 ]; then
	echo "You have to pass the maximum allowed memory usage (in MB) as a command line argument."
	exit 1
elif [ $# -gt 1 ]; then
	echo "Too many arguments"
	exit 1
elif [ $1 = "-h" ] || [ $1 = "--help" ]; then
	echo "Pass maximum allowed memory usage (in MB) as a command line argument."
	exit 0
fi

# Max number of processes allowed, as given by first argument
# the max_MB_per_file argument used to simulate data
# Defaults to 500
# OBS: You need to change it in the individual files. This variable is only
# used to calculate max number of processes that can run at a time.
MAXMBPERFILE=500

if [ $1 -lt $MAXMBPERFILE ]; then
	echo "Maximum allowed memory usage (in MB) must be greater than the maximum file size."
	echo "You allowed max memory usage" $1 "MB and max file size" $MAXMBPERFILE MB.
	exit 1
fi

# Max number of processes that can run at a time, given by your desired
# max memory usage divided by MAXMBPERFILE.
PROCMAX=$(($1 / MAXMBPERFILE))
echo PROCMAX $PROCMAX
# Hash map to save PIDs for each process
declare -A PIDS
# Counter of number of simulations (this will only be allowed to decrease
# when they are processed to get tpr_fpr files)
SIMCOUNT=0
# Loops through simulation types
for SIMTYPE in all single; do
	# Loops through parameters (lines in the relevant params file)
	while read PARAMS; do
		STARTED=false
		echo PARAMS $PARAMS
		while ! $STARTED; do
			echo WAITING FOR STARTING $PARAMS
			echo SIMCOUNT $SIMCOUNT
			echo PROCESSES $(ps | grep R | wc -l)
			if [ $(ps | grep R | wc -l) -lt $PROCMAX ]; then
				# Runs script generating data and saves output in a .output[details] file.
				Rscript run_scripts/${SIMTYPE}targets_sim.R $PARAMS > run_scripts/.output_${SIMTYPE}targets_$(echo $PARAMS | tr " " "_").txt &
				# Saves PID for the script
				PIDS[$SIMTYPE$PARAMS]=$!
				SIMCOUNT=$((SIMCOUNT + 1))
				STARTED=true
			else
				# Waits for a bit before checking again whether 
				# one of the processes are finished
				sleep 10
			fi
		done
	done <<< $(cat $(printf "run_scripts/%stargets_params.txt" $SIMTYPE))
done

# Keeps track of when the different simulations end and immediately starts processing the data.
# Stops looping when all tpr_fpr scripts have been started (i.e, when all simulations of data are done)
while [ $SIMCOUNT -gt 0 ]; do
	for SIMTYPE in all single; do
		# Loops through parameters
		while read PARAMS; do
			echo PARAMS $PARAMS
			echo SIMCOUNT $SIMCOUNT
			# Checks whether the simulaiton is done and has not been processed yet
			if [ ${PIDS[$SIMTYPE$PARAMS]} != 0 ] && ! kill -0 ${PIDS[$SIMTYPE$PARAMS]} > /dev/null; then
				for METHOD in $(ls run_scripts/tpr_fpr_methods); do
					STARTED=false
					echo WAITING FOR STARTING $METHOD
					while ! $STARTED; do
						echo PROCESSES $(ps | grep R | wc -l)
						if [ $(ps | grep R | wc -l) -lt $PROCMAX ]; then
							Rscript run_scripts/tpr_fpr_methods/$METHOD $SIMTYPE $PARAMS > run_scripts/.output_${METHOD}_$(echo $PARAMS | tr " " "_").txt &
							STARTED=true
						else
							# Waits before checking again
							sleep 10
						fi
					done
				done
				SIMCOUNT=$((SIMCOUNT - 1))	
				PIDS[$SIMTYPE$PARAMS]=0
			fi
		done <<< $(cat $(printf "run_scripts/%stargets_params.txt" $SIMTYPE))
	done
	# Waits for a bit before checking again whether any processes are done
	sleep 10 
done

# Waits until all tpr_fpr scripts have finished.
wait
# Then processes the tpr_fpr files to produce ROC_points and AUC files.
Rscript run_scripts/alltargets_AUC.R > run_scripts/.output_alltargets_AUC.txt &
Rscript run_scripts/singletargets_AUC.R > run_scripts/.output_singletargets_AUC.txt &

