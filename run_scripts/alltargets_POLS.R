source("tools/AUC_tools.R")

add_missing_tpr_fpr(beta_POLS, "POLS-coef", sim_type = "alltargets")

add_missing_tpr_fpr(one_minus(p_values_POLS), "POLS-pvals", sim_type = "alltargets")