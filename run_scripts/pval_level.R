source('tools/methods.R')

DAG_list <- readRDS('/docs/Documents/projects/bachelorprojekt/Rcode/data/nx5_local/alltargets_10_5_sdw7_sdh5/1.rds')
n_DAGs <- length(DAG_list)
## OLS, POLS, DPOLS
false_pos_pa_count <- rep(0, 3)
false_pos_anc_count <- rep(0, 3)
pos_count <- rep(0, 3)
method_names <- c('OLS', 'POLS', 'DPOLS')
names(false_pos_pa_count) <- method_names
names(false_pos_anc_count) <- method_names
names(pos_count) <- method_names
not_pa_count <- 0
not_anc_count <- 0
x_count <- 0
## Significance level to test
level <- 0.01
for (i in 1:n_DAGs) {
    DAG <- DAG_list[[i]]
    nx <- length(DAG$x)
    x_count <- x_count + nx
    not_pa_count <- not_pa_count + length(setdiff(DAG$x, DAG$pa_y))
    not_anc_count <- not_anc_count + length(setdiff(DAG$x, DAG$anc_y))
    for (k in 1:nx) {
        for (method in method_names) {
            p_vals <- do.call(paste('p_values_', method, sep = ''), list(dat = DAG$dat))
            if (p_vals[k] < level) {
                pos_count[method] <-
                    pos_count[method] + 1
                if (! DAG$x[k] %in% DAG$anc_y) {
                    false_pos_anc_count[method] <-
                        false_pos_anc_count[method] + 1
                }
                if (! DAG$x[k] %in% DAG$pa_y) {
                    false_pos_pa_count[method] <-
                        false_pos_pa_count[method] + 1
                }
            }
        }
    }
}

false_pos_pa_count / pos_count
false_pos_anc_count / pos_count
not_pa_count / pos_count
not_anc_count / pos_count
