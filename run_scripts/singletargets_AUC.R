source("tools/AUC_tools.R")

add_missing_ROC_points(sim_type = "singletargets")

add_missing_AUC(sim_type = "singletargets")

collect_AUC(sim_type = "singletargets")

