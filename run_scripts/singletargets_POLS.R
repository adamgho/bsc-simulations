source("tools/AUC_tools.R")

add_missing_tpr_fpr(beta_POLS, "POLS-coef", sim_type = "singletargets")

add_missing_tpr_fpr(p_values_POLS, "POLS-pvals", sim_type = "singletargets")