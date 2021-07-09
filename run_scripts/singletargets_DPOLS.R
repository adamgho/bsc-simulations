source("tools/AUC_tools.R")

# Adds tpr_fpr for DPOLS (both based on size of coefficients and p-values)

add_missing_tpr_fpr(beta_DPOLS, "DPOLS-coef", sim_type = "singletargets")

add_missing_tpr_fpr(one_minus(p_values_DPOLS), "DPOLS-pvals", sim_type = "singletargets")