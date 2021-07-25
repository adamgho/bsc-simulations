source("tools/AUC_tools.R")

# Adds tpr_fpr for POLS (both based on size of coefficients and p-values)

add_missing_tpr_fpr(beta_POLS, "POLS-coef", sim_type = "singletargets")

add_missing_tpr_fpr(one_minus(p_values_POLS), "POLS-pvals", sim_type = "singletargets")