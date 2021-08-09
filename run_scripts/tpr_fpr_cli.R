## A function for calculating tpr_fpr. The setting first command line argument
## should be either 'all' or 'single' (denoting alltargets resp. singletargets),
## and the next arguments should be the parameters for the specific setting. The
## order_func and method_name must be provided directly to the function (not as
## command line arguments).

data_dir <- scan('run_scripts/data_dir.txt',
                 what = 'char',
                 quiet = TRUE)
save_tpr_fpr_cli_args <- function(order_func, method_name,
                                    n_DAGs_to_process = Inf)
{
    args <- commandArgs(trailingOnly = TRUE)
    sim_type <- args[1]
    params <- as.numeric(args[2:length(args)])
    if (sim_type == "all") {
        dir <- sprintf("%s/alltargets_%d_%d_sdw%d_sdh%d",
                        data_dir, params[1], params[2], params[3], params[4])
    } else if (sim_type == "single") {
        dir <- sprintf("%s/singletargets_%d_%d_%d_%d_sdw%d_sdh%d",
                       data_dir,
                       params[1], params[2], params[3],
                       params[4], params[5], params[6])
    }
    if (dir.exists(dir) & !file.exists(paste(dir, "/tpr_fpr_", method_name, ".rds", sep = ""))) {
        source("tools/AUC_tools.R")
        save_tpr_fpr(dir, order_func, method_name,
                        n_DAGs_to_process)
    }
}
