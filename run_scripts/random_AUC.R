## Calculates AUC for the random baseline methods used in thesis.

source('tools/AUC_tools.R')

DAG_list <- readRDS('local_results/DAGs1000_nx30_nh30_probconnect04.rds')
n_selects <- 0:30

## Shuffles the given vector
shuffle_vec <- function(vec) {
    if (length(vec) == 1) vec
    else sample(vec)
}

## First correctly finds all parents, then guesses randomly for the rest.
## Useful for seeing whether the methods are only good at finding parents, or
## also good at finding the rest of the ancestors after selecting all parents.
AUC_pa <- rep(NA, length(DAG_list))
AUC_anc <- rep(NA, length(DAG_list))
for (i in 1:length(DAG_list)) {
    DAG_data <- DAG_list[[i]]
    x_pa_y <- intersect(DAG_data$x, DAG_data$pa_y)
    x_not_pa_y <- setdiff(DAG_data$x, x_pa_y)
    ## First select all parents in random order, then select the non-parents
    ## in random order
    x_order <- c(shuffle_vec(x_pa_y),
                 shuffle_vec(x_not_pa_y))
    tpr_fpr_mat <- matrix(NA, ncol = 4, nrow = length(n_selects))
    for (k in 1:length(n_selects)) {
        n_select <- n_selects[k]
        if (n_select == 0) selected <- NULL
        else selected <- x_order[1:n_select]
        tpr_fpr_mat[k, ] <- tpr_fpr_vec(selected, DAG_data)
    }
    AUC_pa[i] <- get_AUC(tpr_fpr_mat[ , 3],
                         tpr_fpr_mat[ , 4])
    AUC_anc[i] <- get_AUC(tpr_fpr_mat[ , 1],
                          tpr_fpr_mat[ , 2])
}

write.table(tibble(AUC_pa, AUC_anc),
            'local_results/pa_then_random_all_AUC.txt',
            row.names = FALSE)

write.table(tibble(AUC_pa_mean = mean(AUC_pa),
                   AUC_pa_median = median(AUC_pa),
                   AUC_pa_quartile1 = quartile1(AUC_pa),
                   AUC_pa_quartile3 = quartile3(AUC_pa),
                   AUC_anc_mean = mean(AUC_anc),
                   AUC_anc_median = median(AUC_anc),
                   AUC_anc_quartile1 = quartile1(AUC_anc),
                   AUC_anc_quartile3 = quartile3(AUC_anc)),
            'local_results/pa_then_random_AUC_summary.txt',
            row.names = FALSE)

## Selects everything randomly
AUC_pa <- rep(NA, length(DAG_list))
AUC_anc <- rep(NA, length(DAG_list))
for (i in 1:length(DAG_list)) {
    DAG_data <- DAG_list[[i]]
    ## Orders the X's randomly
    x_order <- c(shuffle_vec(DAG_data$x))
    tpr_fpr_mat <- matrix(NA, ncol = 4, nrow = length(n_selects))
    for (k in 1:length(n_selects)) {
        n_select <- n_selects[k]
        if (n_select == 0) selected <- NULL
        else selected <- x_order[1:n_select]
        tpr_fpr_mat[k, ] <- tpr_fpr_vec(selected, DAG_data)
    }
    AUC_pa[i] <- get_AUC(tpr_fpr_mat[ , 3],
                         tpr_fpr_mat[ , 4])
    AUC_anc[i] <- get_AUC(tpr_fpr_mat[ , 1],
                          tpr_fpr_mat[ , 2])
}

write.table(tibble(AUC_pa, AUC_anc),
            'local_results/all_random_all_AUC.txt',
            row.names = FALSE)

write.table(tibble(AUC_pa_mean = mean(AUC_pa),
                   AUC_pa_median = median(AUC_pa),
                   AUC_pa_quartile1 = quartile1(AUC_pa),
                   AUC_pa_quartile3 = quartile3(AUC_pa),
                   AUC_anc_mean = mean(AUC_anc),
                   AUC_anc_median = median(AUC_anc),
                   AUC_anc_quartile1 = quartile1(AUC_anc),
                   AUC_anc_quartile3 = quartile3(AUC_anc)),
            'local_results/all_random_AUC_summary.txt',
            row.names = FALSE)


## Selects everything randomly for the 5 X 5 H case

DAG_list <- readRDS('local_results/DAGs1000_nx5_nh5_probconnect04.rds')
n_selects <- 0:5

AUC_pa <- rep(NA, length(DAG_list))
AUC_anc <- rep(NA, length(DAG_list))
for (i in 1:length(DAG_list)) {
    DAG_data <- DAG_list[[i]]
    ## Orders the X's randomly
    x_order <- c(shuffle_vec(DAG_data$x))
    tpr_fpr_mat <- matrix(NA, ncol = 4, nrow = length(n_selects))
    for (k in 1:length(n_selects)) {
        n_select <- n_selects[k]
        if (n_select == 0) selected <- NULL
        else selected <- x_order[1:n_select]
        tpr_fpr_mat[k, ] <- tpr_fpr_vec(selected, DAG_data)
    }
    AUC_pa[i] <- get_AUC(tpr_fpr_mat[ , 3],
                         tpr_fpr_mat[ , 4])
    AUC_anc[i] <- get_AUC(tpr_fpr_mat[ , 1],
                          tpr_fpr_mat[ , 2])
}

write.table(tibble(AUC_pa, AUC_anc),
            'local_results/nx5_all_random_all_AUC.txt',
            row.names = FALSE)

write.table(tibble(AUC_pa_mean = mean(AUC_pa),
                   AUC_pa_median = median(AUC_pa),
                   AUC_pa_quartile1 = quartile1(AUC_pa),
                   AUC_pa_quartile3 = quartile3(AUC_pa),
                   AUC_anc_mean = mean(AUC_anc),
                   AUC_anc_median = median(AUC_anc),
                   AUC_anc_quartile1 = quartile1(AUC_anc),
                   AUC_anc_quartile3 = quartile3(AUC_anc)),
            'local_results/nx5_all_random_AUC_summary.txt',
            row.names = FALSE)
