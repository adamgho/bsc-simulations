source("tools/AUC_tools.R")

# Adds tpr_fpr for ICP

add_missing_tpr_fpr(one_minus(p_values_ICP),
                    "ICP",
                    sim_type = "singletargets",
                    n_DAGs_to_process = 100)