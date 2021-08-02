#!/usr/bin/env Rscript

## Simulates from singletargets with parameters given by the six command line
## arguments

# Command line arguments
params <- as.numeric(commandArgs(trailingOnly = TRUE))

## Filename of DAG list to simulate from
DAGs_filename <- "data/DAGs1000_nx30_nh30_probconnect04.rds"
##DAGs_filename <- "data/DAGs1000_nx5_nh5_probconnect04.rds"

# Indices of the DAGs in the list to simulate from
DAGs_to_sim <- 1:1000

## directory to save files in
dir <- scan('run_scripts/data_dir.txt',
            what = 'char',
            quiet = TRUE)

# Only loads libraries and runs if the directory doesn't already exist.
if (!dir.exists(sprintf("%s/singletargets_%d_%d_%d_%d_sdw%d_sdh%d",
                        dir,
                        params[1], params[2], params[3],
                        params[4], params[5], params[6])))
{
    source(scan('run_scripts/data_tools_file.txt',
                what = 'char',
                quiet = TRUE))
    DAG_list <- readRDS(DAGs_filename)[DAGs_to_sim]
    sim_singletargets_datasets(
        DAG_list,
        params[1], params[2], params[3], params[4], params[5], params[6],
        dir = dir, max_MB_per_file = 500
    )
}
