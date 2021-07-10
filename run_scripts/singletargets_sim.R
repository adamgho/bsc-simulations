#!/usr/bin/env Rscript

params <- as.numeric(commandArgs(trailingOnly = TRUE))

# Only loads libraries and runs if the directory doesn't already exist.
if (!dir.exists(sprintf("data/singletargets_%d_%d_%d_%d_sdw%d_sdh%d",
                        params[1], params[2], params[3],
                        params[4], params[5], params[6])))
{
    source("tools/data_tools.R")
    DAG_list <- readRDS("data/DAGs1000_nx30_nh30_probconnect04.rds")
    sim_singletargets_datasets(
        DAG_list,
        params[1], params[2], params[3], params[4], params[5], params[6],
        dir = "data/", max_MB_per_file = 500
    )
} 