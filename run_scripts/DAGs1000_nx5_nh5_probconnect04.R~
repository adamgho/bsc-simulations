source("tools/DAG_tools.R")

set.seed(210623)

DAG_list <- list()
n_DAGs <- 1000

for (i in 1:n_DAGs) {
  if (i %% 50 == 0) print(i)
  DAG_list <- c(DAG_list, list(get_random_DAG(n_x = 30,
                                                 n_h = 30,
                                                 prob_connect = 0.4)))
}

saveRDS(DAG_list, "data/DAGs1000_nx30_nh30_probconnect04.rds")


