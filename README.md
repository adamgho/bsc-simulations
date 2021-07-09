# Causal discovery from interventional data

This repository contains code for reproducing -- or expanding upon -- simulation results from my BSc thesis.

## Prerequisites

Create

- a subdirectory named `data`. This will store a lot of data (hundreds of GB), so personally I mounted an external HDD at this directory.
- a subdirecte named `plots`.

You must have `R` installed.

Run `Rscripts run_scripts/install_packages.R` to install the necessary R packages.

I have only tested the code on GNU/Linux. I assume that it works on macOS as well, but am unsure whether it works on Windows, since I use `system2` to call `mv` to rename a directory.

## Reproducing my results

Note: I didn't set a seed for practical reason (starting and stopping simulations many times over several weeks), so you will not get the exact same data and plots.

Run `Rscript run_scripts/DAGs1000_nx30_nh30_probconnect04.R` to generate and save random DAGs.

### Parallelized

In the following I assume you have 10 cores, so replace 10 by the actual number of cores at your disposal.


1. Split `alltargets_params_complete.txt` into 10 files named `alltargets_params1.txt`, ..., `alltargets_params10.txt`.
2. Run `Rscript run_scripts/alltargets_data 1` on core 1, ..., `Rscript run_scripts/alltargets_data 10` on core 10.
3. Split `singletargets_params_complete.txt` into m/2 files named `singletargets_params1.txt`, ..., `singletargets_params10.txt`.
4. Run `Rscript run_scripts/singletargets_data 1` on core 1, ..., `Rscript run_scripts/singletargets_data 10` on core 10.
5. Run the following (ideally give each its own core).
	- `Rscript run_scripts/alltargets_OLS.R`
	- `Rscript run_scripts/alltargets_POLS.R`
	- `Rscript run_scripts/alltargets_DPOLS.R`
	- `Rscript run_scripts/alltargets_ICP.R`
	- `Rscript run_scripts/singletargets_OLS.R`
	- `Rscript run_scripts/singletargets_POLS.R`
	- `Rscript run_scripts/singletargets_DPOLS.R`
	- `Rscript run_scripts/singletargets_ICP.R`
	- `Rscript run_scripts/singletargets_t.R` (TODO)
6. Run the following on two cores.
	- `Rscript run_scripts/alltargets_AUC.R`
	- `Rscript run_scripts/singletargets_AUC.R`
7. Run `Rscripts run_scripts/generate_plots.R`.
8. You are done!

### Unparallelized --- easy, but slow!

To run the simulations unparallelized (which will take a *very* long time) run

1. Create a subdirectory named "data". This will store a lot of data (hundreds of GB), so personally I mounted an external HDD at this directory.
2. Create a subdirectory named "plots".
3. Run `run_scripts/run_complete.sh` while standing in the root directory.
4. You are done!

## Overview of code

### `run_scripts`

`run_scripts` contains user interfaces for easily simulating large amounts of data.

### `tools`

`tools` contains the main tools for simulating random SCMs, data from them, and running the methods.

- `DAG_tools.R` for simulating random DAGs.
- `data_tools.R` for simulating random linear Gaussian SCMs and data from them.
- `methods.R` for getting parameter estimates and p-values from the different methods.
- `AUC_tools.R` for calculating area under the curve for the methods on simulated data.
