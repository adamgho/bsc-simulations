source("tools/AUC_tools.R")

add_missing_tpr_fpr(beta_OLS, "OLS-coef", sim_type = "alltargets")

add_missing_tpr_fpr(one_minus(p_values_OLS), "OLS-pvals", sim_type = "alltargets")