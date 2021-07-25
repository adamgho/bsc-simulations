source('tools/methods.R')
source('tools/AUC_tools.R')

data_dir <- scan('run_scripts/data_dir.txt',
                 what = 'char',
                 quiet = TRUE)

add_missing_tpr_fpr(get_meanshifts, 'mean-shift',
                    'singletargets', n_DAGs_to_process = Inf, 
                    dir = data_dir)
