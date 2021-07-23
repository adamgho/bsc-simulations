source("tools/AUC_tools.R")

# Adds ROC and AUC files based on tpr_fpr files.

add_missing_AUC(sim_type = "singletargets")

collect_AUC(sim_type = "singletargets")

