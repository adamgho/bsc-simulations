#!/usr/bin/env Rscript

## Command line arguments
params <- as.numeric(commandArgs(trailingOnly = TRUE))

## Indices of DAGs to simulate from
DAGs_to_sim <- 1:100
## DAGs_filename <- "data/DAGs1000_nx30_nh30_probconnect04.rds"
DAGs_filename <- "data/DAGs1000_nx5_nh5_probconnect04.rds" 
## DAGs_filename <- "data/exampleDAG1.rds"

## directory to save files in
dir <- scan('run_scripts/data_dir.txt',
            what = 'char',
            quiet = TRUE)

## Only loads libraries and runs if the directory doesn't already exist.
if (!dir.exists(sprintf("%s/alltargets_%d_%d_sdw%d_sdh%d",
                        dir, params[1], params[2], params[3], params[4])))
{
    source(scan('run_scripts/data_tools_file.txt',
                what = 'char',
                quiet = TRUE))
    DAG_list <- readRDS(DAGs_filename)[DAGs_to_sim]
    sim_alltargets_datasets(
        DAG_list, params[1], params[2], params[3], params[4],
        dir = dir, max_MB_per_file = 500
    )
}


  
