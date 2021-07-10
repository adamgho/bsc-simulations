save_tpr_fpr_cli_args <- function(order_func, method_name,
                                    n_DAGs_to_process)
{
    args <- commandArgs(trailingOnly = TRUE)
    sim_type <- args[1]
    params <- as.numeric(args[2:length(args)])
    if (sim_type == "all") {
        dir <- sprintf("data/alltargets_%d_%d_sdw%d_sdh%d",
                        params[1], params[2], params[3], params[4])
    } else if (sim_type == "single") {
       dir <- sprintf("data/singletargets_%d_%d_%d_%d_sdw%d_sdh%d",
                    params[1], params[2], params[3],
                    params[4], params[5], params[6])
    }

    if (!file.exists(paste(dir, "/tpr_fpr_", method_name, ".rds", sep = ""))) {
        source("tools/AUC_tools.R")
        save_tpr_fpr(dir, order_func, method_name,
                        n_DAGs_to_process)
    }
}