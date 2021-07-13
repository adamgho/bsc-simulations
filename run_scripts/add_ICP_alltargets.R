source('tools/methods.R')
source('tools/AUC_tools.R')

add_missing_tpr_fpr(one_minus(p_values_ICP), 'ICP',
                    'alltargets', n_DAGs_to_process = 10, 
                    dir = 'data/data_for_ICP')