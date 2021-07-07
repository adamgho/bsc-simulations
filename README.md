# Causal discovery from interventional data

This repository contains code for reproducing -- or expanding upon -- simulation results from my BSc thesis.

## Quick start guide

1. Create a directory named "data". This will store a lot of data (hundreds of GB), so personally I mounted an external HDD at this directory.
2. Run `run_scripts/alltargets_complete.sh` while in the root directory to generate all data for alltargets. I highly recommend parallelizing as much as possible; look inside the script to get an idea of how to parallelize.

## `tools`

`tools` contains the main tools for simulating random SCMs, data from them, and running the methods.

- `AUC_tools.R` for calculating area under the curve for the methods on simulated data.
- `DAG_tools.R` for simulating random DAGs.
- `data_tools.R` for simulating random linear Gaussian SCMs and data from them.
- `methods.R` for getting parameter estimates and p-values from the different methods.

## `run_scripts`

`run_scripts` contains scripts you should run to reproduce the data and plots from my BSc thesis. I didn't set seeds (due to having to run the simulations on and off on my laptop over many days) so you will not get the exact same data. This should ideally be parallelized.
The following scripts are included:

- alltargets_complete.sh This generates all data from the alltargets setup.
- alltargets_tpr_fpr.sh This adds tpr_fpr files containing true positive rates for
	all methods considered on alltargets.

## `data` (create this yourself before running scripts)

`data` will contain all the simulated data, and the raw results of the analyses, once you have run the scripts in `run_scripts`.

`data` will contain directories for each choice of simulation parameters. Each directory starts by indicating whether it is from the alltargets or singletargets setup. Afterwards several numbers give the parameters.

## `plots` (create this yourself before running scripts)

`plots` will contain all the plots once you have run the scripts in `run_scripts`.
