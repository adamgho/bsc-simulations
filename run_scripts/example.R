source("tools/DAG_tools.R")
source('tools/data_tools.R')
source('tools/methods.R')

iG = graph_from_edgelist(matrix(c(
    1, 2,
    1, 5,
    3, 4,
    4, 5,
    5, 7,
    6, 7),
    byrow = TRUE,
    ncol = 2)
)
y = 5
x = c(3, 4, 2, 6, 7)
h = 1
G = as_adjacency_matrix(iG)
B = randomB(G)
anc_y <- anc(iG, y)
pa_y <- pa(iG, y)

DAG1 <- structure(mget(
    c('iG', 'y', 'x', 'h', 'G', 'B', 'pa_y', 'anc_y')),
    class = 'DAG_data')
plot(DAG1)

DAG1 <- sim_alltargets(DAG1, 2, 1000, 7, 5)

beta_mat <- cbind(beta_OLS(DAG1$dat),
    beta_POLS(DAG1$dat),
    beta_DPOLS(DAG1$dat))


order_mat <- matrix(NA, nrow = nrow(beta_mat), ncol = ncol(beta_mat))
pvals_mat <- cbind(p_values_OLS(DAG1$dat),
                    p_values_POLS(DAG1$dat),
                    p_values_DPOLS(DAG1$dat))

for (i in 1:3) {
        order_mat[, i] <- DAG1$x[rev(order(abs(beta_mat[, i])))]
}

order_mat
pvals_mat
beta_mat

DAG_list <- readRDS('data/old_data/alltargets_10_500_sdw7_sdh5/1.rds')
ROC_points <- readRDS('data/old_data/alltargets_10_500_sdw7_sdh5/ROC_points.rds')
DAG2 <- DAG_list[[1]]
plot(DAG2)
DAG2
plot(DAG2$iG, vertex.size=10, arrow.size=1)

beta_mat <- cbind(beta_OLS(DAG2$dat),
    beta_POLS(DAG2$dat),
    beta_DPOLS(DAG2$dat))


order_mat <- matrix(NA, nrow = nrow(beta_mat), ncol = ncol(beta_mat))
pvals_mat <- cbind(p_values_OLS(DAG2$dat),
                    p_values_POLS(DAG2$dat),
                    p_values_DPOLS(DAG2$dat))

for (i in 1:3) {
        order_mat[, i] <- DAG2$x[rev(order(abs(beta_mat[, i])))]
}

order_mat
pvals_mat
beta_mat

tkplot(iG)

DAG2$x
