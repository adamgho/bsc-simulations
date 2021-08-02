# Causal discovery from interventional data

This repository contains code for reproducing -- or expanding upon -- simulation
results from my BSc thesis.

## Prerequisites

You must have `R` and `bash` installed. Every script has to be run while
standing in the root of the repository.

Run `Rscript run_scripts/install_packages.R` to install the necessary R packages.

I have only tested the code on GNU/Linux. I assume that it works on other
Unix-like systems as well (e.g., macOS or FreeBSD), but am unsure whether it works on
Windows.

## Preparing for simulations

Create a subdirectory for the data and set the file `run_tools/data_dir.txt` to contain
its name (if you just call the subdirectory `data`, then
`run_tools/data_dir.txt` already contains the right name).

If you want to simulate from the Problem A setting, then set the content of
`run_tools/data_tools_file.txt` to be `tools/separate_data_tools.R` (i.e., run
`echo 'tools/separate_data_tools.R' > run_tools/data_tools_file.txt`), or if you
want to simulate form the Problem B setting, then set the content of
`data_tools_file.txt` to be `tools/data_tools.R` (i.e., run `echo
'rools/data_tools.R > run_tools/data_tools_file.txt`).

## Prerequisites

Create two subdirectories:

- A subdirectory named `data`. This will store a lot of data (hundreds of GB), so personally I mounted an external HDD at this directory.
- A subdirectory named `plots`.

You must have `R` and `bash` installed.

Run `Rscripts run_scripts/install_packages.R` to install the necessary R packages.

I have only tested the code on GNU/Linux. I assume that it works on macOS as well, but am unsure whether it works on Windows.

## Reproducing my results

Note: I didn't set a seed for practical reason (starting and stopping simulations many times over several weeks), so you will not get the exact same data and plots.

First run `Rscript run_scripts/DAGs1000_nx30_nh30_probconnect04.R` to generate and save a list of the random DAGs used.

Now run the script `./run_scripts/run_in_background.sh <maxMB>` where you replace `<maxMB>` with the maximum allowed memory usage in MB, e.g `./run_sim_and_tpr_fpr_and_AUC.sh 50000` to allow a maximum memory usage of 50000 MB (50 GB). It is quite unprecise so be conservative. (For alternative command line arguments, run it with the `-h` argument.)
To monitor the running processes you can run `./run_scripts/cont_list_running.sh`. If you have to stop all processes then run `./run_scripts/kill_all.sh`.

Finally run `Rscript run_scripts/generate_plots.R`.

## Overview

### `run_scripts`

`run_scripts` contains user interfaces for easily simulating large amounts of data. The script `run_sim_and_tpr_fpr_and_AUC.sh` is particularly useful.It takes a command line argument giving the maximum allowable memory usage in MB. It parses the files `alltargets_params.txt` and `singletargets_params.txt` where each line contains a set of parameters. It then simulates data from all of these setups in parallel, attempting to use less memory than the argument passed by the user. After any of the data simulations are done (while the others might still run) it starts processing the data to save true positive and false positive rates. When all of the data is simulated an processed in this way it does a last pass through the folder to collect all information and calculate AUC.

### `tools`

`tools` contains the main tools for simulating random SCMs, data from them, and running the methods.

- `DAG_tools.R` for simulating random DAGs.
- `data_tools.R` for simulating random linear Gaussian SCMs and data from them.
- `methods.R` for getting parameter estimates and p-values from the different methods.
- `AUC_tools.R` for calculating area under the curve for the methods on simulated data.

### `LICENCE.md`

Contains license terms.
