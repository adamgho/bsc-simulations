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

Run `run_scripts/DAGs1000_nx30_nh30_probconnect04.R` respectively
`DAGs1000_nx5_nh5_probconnect04.R` to simulate DAGs with 30 X and 30 H
respectively 5 X and 5 H. Change the `saveRDS` in those files to save the DAGs
where you wish.

Create a subdirectory named for the data and set the file
`run_tools/data_dir.txt` to contain its name (if you just call the subdirectory
`data`, then `run_tools/data_dir.txt` already contains the right name).

If you want to simulate from the Problem A setting, then set the content of
`run_tools/data_tools_file.txt` to be `tools/separate_data_tools.R` (i.e., run
`echo 'tools/separate_data_tools.R' > run_tools/data_tools_file.txt`), or if you
want to simulate form the Problem B setting, then set the content of
`data_tools_file.txt` to be `tools/data_tools.R` (i.e., run `echo
'rools/data_tools.R > run_tools/data_tools_file.txt`).

Make sure that the directory `run_scripts/tpr_fpr_methods` contains exactly the
methods you want to run after simulating data.

Add simulation parameters to `run_scripts/alltargets_params.txt` and
`run_scripts/singletargets_params.txt` (they must contain at least one line
each). Each line in `alltargets_params.txt` must be of the form `a b c d`
where `a` is the number of observations per environment, `b` is the number of
environments, `c` is the standard deviation of the mean shifts, and `d` is the
standard deviation of the hiddens. Each line in `singletargets_params.txt` must
be of the form `e f g h i j` where `e` is the number of observations per
environment, `f` is the number of environments, `g` is the number of X's to
intervene on, `h` is the number of control observations, `i` is the standard
deviation of the mean shifts, and `j` is the standard deviation of the hidden variables.

Go to `run_scripts/alltargets_sim.txt` respectively
`run_scripts/singletargets_sim.txt` and set the `DAGs_to_sim` variable to
contain the indices of the DAGs you wish to simulate from, and set the
`DAGs_filename` to contain the file containing the list of DAGs.

## Running simulations and analyzing the data

Run `run_scripts/run_sim_and_tpr_fpr_and_AUC.sh -p k` where `k` should be the
number of parallel processes you want to allow. This will simulate data, apply
the methods to the data, and process the results to calculate AUC. Nothing more
is needed! The script automatically keeps track of when the simulations are
done, and add the corresponding analysis of the data to the queue at the
appropriate time.

If you have to stop the simulations again and want to continue at a later time,
then juct use `Ctrl-C` to stop them, and run the command
`run_scripts/run_sim_and_tpr_fpr_and_AUC.sh -p k` again when you are ready to
start them again. When simulating data, the simulated data sets are saved
in files of approximately 500 MB, and the simulation will automatically pick up
from where it left off when you restart the script.

While a simulation is running, you may wish to check which processes are
currently running to get an idea of how far it has come. To do this, just run
`run_scripts/cont_list_running.sh` in another terminal.

## Other scripts for the thesis

The script `run_scripts/random_AUC.R` calculates the mean and quartiles of the
random baseline methods. You must supply a list of DAGs for it; see the source code.

The script `run_scripts/generate_plots.R` was used to generate the plots. You
will not be able to run this without modifying it yourself. Due to the large
amount of simulation (over 4.5 TB of data in total) I split them up in six
separate portions, named stor1, ..., stor6, stored in subdirectories of a
directory I named local_results. The figures are saved as tikz-code in tex-files
in the directory `~/thesis/figures/`.

### Not included in the thesis

`pval_level.R` is a rough sketch for checking the levels of the p-values -- it
indicates that they don't hold level, but this was not explicitly included in
the thesis.

## Overview of folders

### `run_scripts`

`run_scripts` contains user interfaces for easily simulating large amounts of
data. The script `run_sim_and_tpr_fpr_and_AUC.sh` is particularly useful. It
parses the files `alltargets_params.txt` and `singletargets_params.txt` where
each line contains a set of parameters. It then simulates data from all of these
setups in parallel. After any of the data simulations are done (while the others
might still run) it starts processing the data to save true positive and false
positive rates. When all of the data is simulated an processed in this way it
does a last pass through the folder to collect all information and calculate
AUC.

### `tools`

`tools` contains the main tools for simulating random SCMs, data from them, and running the methods.

- `DAG_tools.R` for simulating random DAGs.
- `separate_data_tools.R` for simulating random linear Gaussian SCMs and data
  from them in the Problem A setup.
- `data_tools.R` for simulating random linear Gaussian SCMs and data from them
  in the Problem B setup.
- `methods.R` for getting parameter estimates and p-values from the different methods.
- `AUC_tools.R` for calculating area under the curve for the methods on simulated data.

### `LICENCE.md`

Contains license terms.
