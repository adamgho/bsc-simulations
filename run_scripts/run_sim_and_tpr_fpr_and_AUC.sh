#!/bin/bash

# This script simulates and processes all data setups given in the files
# alltargets_params.txt and singletargets_params.txt.
# Each line in the files should contain the parameters for the given
# simulation setup, in the order in which they are passed to the
# sim_alltargets respectively sim_singletargets functions.

# Time to sleep between each check
SLEEPTIME=.1

# Max size of each file
# OBS: You need to change it in the individual files. This variable is only
# used to calculate max number of processes that can run at a time.
MAXMBPERFILE=500



echo STARTING > "run_scripts/.output/run.txt"

if [ $# = 0 ]; then
	echo "You need to pass an argument. Run with -h argument for help."
	exit 1
elif [ $# -gt 3 ]; then
	echo "Too many arguments. Run with -h argument for help."
	exit 1
elif [ $1 = "-h" ] || [ $1 = "--help" ]; then
	echo "Choose between the following command line arguments."
	echo "E.g running with argument -p 4 means runnings a maximum of 4 processes."
	echo "Running with argument 16000 (equivalent to -m 16000) means using a maximum of 16000 MB of memory (approximately; this can be quite far off, so try to be conservative)."
	echo "-m (--memory) (default) Maximum allowed memory usage (in MB)."
	echo "-p (--processes) Maximum allowed number of processes"
	exit 0
elif [ $1 = "-p" ] || [ $1 = "--processes" ]; then
	PROCMAX=$2
else
	if [ $1 = "-m" ] || [ $1 = "--memory" ]; then
		MAXMEMORY=$2
	else
		MAXMEMORY=$1
	fi
	# Max number of processes allowed, as given by first argument
	# the max_MB_per_file argument used to simulate data
	# Defaults to 500

	if [ $2 -lt $MAXMBPERFILE ]; then
		echo "Maximum allowed memory usage (in MB) must be greater than the maximum file size."
		echo "You allowed max memory usage" $MAXMEMORY "MB and max file size" $MAXMBPERFILE MB.
		exit 1
	fi
	# Max number of processes that can run at a time, given by your desired
	# max memory usage divided by MAXMBPERFILE.
	PROCMAX=$((MAXMEMORY / MAXMBPERFILE))
fi

echo "Max number of processes:" $PROCMAX
echo "Approx. max memory usage:" $((MAXMBPERFILE * PROCMAX))
# Hash map to save PIDs for each process
declare -A PIDS
# Counter of number of simulations (this will only be allowed to decrease
# when they are processed to get tpr_fpr files)
SIMCOUNT=0
echo PID WHAT SIMTYPE PARAMS
# Loops through simulation types
for SIMTYPE in all single; do
	# Sorts rows and removes duplicates
	sort -k1 -k2 -n run_scripts/${SIMTYPE}targets_params.txt | uniq > tmp
	mv tmp run_scripts/${SIMTYPE}targets_params.txt
	# Loops through parameters (lines in the relevant params file)
	while read PARAMS; do
		STARTED=false
		#echo PARAMS $PARAMS
		while ! $STARTED; do
			#echo SIMCOUNT $SIMCOUNT
			#echo PROCESSES $(ps | grep R | wc -l)
			if [ $(ps | grep R | wc -l) -lt $PROCMAX ]; then
				# Runs script generating data and saves output in a .output[details] file.
				Rscript run_scripts/${SIMTYPE}targets_sim.R $PARAMS > run_scripts/.output/${SIMTYPE}targets_$(echo $PARAMS | tr " " "_").txt &
				# Saves PID for the script
				echo $! SIM $SIMTYPE $PARAMS | tee -a run_scripts/.output/run.txt
				PIDS[$SIMTYPE$PARAMS]=$!
				SIMCOUNT=$((SIMCOUNT + 1))
				STARTED=true
			else
				# Waits for a bit before checking again whether 
				# one of the processes are finished
				sleep $SLEEPTIME
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
			#echo PARAMS $PARAMS
			echo SIMCOUNT $SIMCOUNT
			# Checks whether the simulaiton is done and has not been processed yet
			if [ ${PIDS[$SIMTYPE$PARAMS]} != 0 ] && ! kill -0 ${PIDS[$SIMTYPE$PARAMS]} &> /dev/null; then
				for METHOD in $(ls run_scripts/tpr_fpr_methods); do
					STARTED=false
					while ! $STARTED; do
						#echo PROCESSES $(ps | grep R | wc -l)
						if [ $(ps | grep R | wc -l) -lt $PROCMAX ]; then
							Rscript run_scripts/tpr_fpr_methods/$METHOD $SIMTYPE $PARAMS > run_scripts/.output/${METHOD}_$(echo $PARAMS | tr " " "_").txt &
							echo $! $METHOD $SIMTYPE $PARAMS | tee -a run_scripts/.output/run.txt
							STARTED=true
						else
							# Waits before checking again
							sleep $SLEEPTIME
						fi
					done
				done
				SIMCOUNT=$((SIMCOUNT - 1))	
				PIDS[$SIMTYPE$PARAMS]=0
				# Keeping track of TPR FPR PIDs
				PIDS2+=($!)
			fi
		done <<< $(cat $(printf "run_scripts/%stargets_params.txt" $SIMTYPE))
	done
	# Waits for a bit before checking again whether any processes are done
	sleep $SLEEPTIME
done

# Waits until all tpr_fpr scripts have finished.
wait "${PIDS2[@]}"
# Then processes the tpr_fpr files to produce ROC_points and AUC files.
Rscript run_scripts/alltargets_AUC.R > run_scripts/.output_alltargets_AUC.txt &
Rscript run_scripts/singletargets_AUC.R > run_scripts/.output_singletargets_AUC.txt &

