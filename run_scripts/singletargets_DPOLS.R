source("tools/AUC_tools.R")

add_missing_tpr_fpr(beta_DPOLS, "DPOLS-coef", sim_type = "singletargets")

add_missing_tpr_fpr(p_values_DPOLS, "DPOLS-pvals", sim_type = "singletargets")