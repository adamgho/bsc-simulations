source("tools/data_tools.R")

DAG_list <- readRDS("data/DAGs1000_nx30_nh30_probconnect04.rds")

# Script to simulate data using sim_alltargets_txt.
# To use: Rscript params_file args
# args can be numbers or strings (see comment below).

for (params_file in commandArgs(trailingOnly = T)) {
    # If a number is passed, e.g 3, then it is assumed that the user
    # means the file "run_scripts/alltargets_params[num].txt",
    # e.g "run_scripts/alltargets_params3.txt". Otherwise the
    # argument is just assumed to be the filename.
    if (str_detect(params_file, "^[:digit:]+$")) {
        params_file <- str_c("run_scripts/alltargets_params",
                                params_file, ".txt")
    }
    sim_alltargets_txt(DAG_list, params_file)
}