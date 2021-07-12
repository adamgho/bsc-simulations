#!/usr/bin/env Rscript

# Command line arguments
params <- as.numeric(commandArgs(trailingOnly = TRUE))

# Indices of DAGs to simulate from
DAGs_to_sim <- 1:10
# DAGs_filename <- "data/DAGs1000_nx30_nh30_probconnect04.rds" 
DAGs_filename <- "data/exampleDAG1.rds"

# Only loads libraries and runs if the directory doesn't already exist.
if (!dir.exists(sprintf("data/alltargets_%d_%d_sdw%d_sdh%d",
                        params[1], params[2], params[3], params[4])))
{
    source("tools/data_tools.R")
    DAG_list <- readRDS(DAGs_filename)[DAGs_to_sim]
    sim_alltargets_datasets(
        DAG_list, params[1], params[2], params[3], params[4],
        dir = "data/", max_MB_per_file = 500
    )
}


  