source("tools/DAG_tools.R")

set.seed(210725)

DAG_list <- list()
n_DAGs <- 1000

for (i in 1:n_DAGs) {
  if (i %% 50 == 0) print(i)
  DAG_list <- c(DAG_list, list(get_random_DAG(n_x = 5,
                                                 n_h = 5,
                                                 prob_connect = 0.4)))
}

saveRDS(DAG_list, "data/DAGs1000_nx5_nh5_probconnect04.rds")


