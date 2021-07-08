source("tools/data_tools.R")

DAG_list <- readRDS("data/DAGs1000_nx30_nh30_probconnect04.rds")

for (params_file in commandArgs(trailingOnly = T)) {
    # If a number is passed, e.g 3, then it is assumed that the user
    # means the file "run_scripts/singletargets_params[num].txt",
    # e.g "run_scripts/singletargets_params3.txt".
    if (str_detect(params_file, "^[:digit:]+$")) {
        params_file <- str_c("run_scripts/singletargets_params",
                                params_file, ".txt")
    }
    sim_singletargets_txt(DAG_list, params_file)
}