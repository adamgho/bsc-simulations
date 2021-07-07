source("tools/AUC_tools.R")

add_missing_ROC_points(sim_type = "alltargets")

add_missing_AUC(sim_type = "alltargets")

collect_AUC(sim_type = "alltargets")

