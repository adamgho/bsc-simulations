source("tools/AUC_tools.R")

add_missing_tpr_fpr(p_values_ICP,
                    "ICP",
                    sim_type = "alltargets",
                    n_DAGs_to_process = 100)