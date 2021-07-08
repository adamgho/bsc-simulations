source("tools/AUC_tools.R")

add_missing_tpr_fpr(beta_DPOLS, "DPOLS-coef", sim_type = "alltargets")

add_missing_tpr_fpr(one_minus(p_values_DPOLS), "DPOLS-pvals", sim_type = "alltargets")