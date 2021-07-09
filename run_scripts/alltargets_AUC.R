source("tools/AUC_tools.R")

# Generates ROC and AUC from tpr_fpr files.

add_missing_ROC_points(sim_type = "alltargets")

add_missing_AUC(sim_type = "alltargets")

collect_AUC(sim_type = "alltargets")

