source('tools/methods.R')
source('tools/AUC_tools.R')
library(InvariantCausalPrediction)

data_dir <- scan('run_scripts/data_dir.txt',
                 what = 'char',
                 quiet = TRUE)

add_missing_tpr_fpr(one_minus(p_values_ICP), 'ICP',
                    'singletargets', n_DAGs_to_process = 10, 
                    dir = data_dir)

add_missing_tpr_fpr(one_minus(p_values_PICP), 'PICP',
                    'singletargets', n_DAGs_to_process = 10,
                    dir = data_dir)
