library(tidyverse)
library(igraph)
library(Matrix)

# Simulates data from the alltargets setup.
# INPUT:
# DAG_data determines the DAG, the structural assignment, and which variables
# are hiddens, X's, and Y.
# num_interv is the number of environments (i.e different interventions).
# shift_noise_sd is the sd of the mean shifts.
# n_obs_each is the number of observations from each environment (setting this
# means that all environments have the same number of observations).
# sd_hiddens is the sd of the hidden variables.
sim_alltargets <- function(DAG_data,
                          n_obs_each,
                          num_interv,
                          shift_noise_sd,
                          sd_hiddens) {
  # The i'th element of n_obs contains the number of observations
  # of the i'th setting.
  n_obs <- rep(n_obs_each, num_interv)
  num_var <- nrow(DAG_data$B)
 
  # The i'th column gives the mean shifts of the X's in the i'th environment
  w <- matrix(
    rnorm(num_interv * length(DAG_data$x), mean = 0, sd = shift_noise_sd),
    nrow = length(DAG_data$x),
    ncol = num_interv)
  
  # Function that simulates one interventional setting.
  # Input: i
  # Output: Tibble with n_obs[i] observations of all variables.
  interv_sim <- function(i) {
    # rows for variables, columns for repetitions.
    # The X's are all standard normal
    N <- matrix(rnorm(n_obs[i] * num_var),
                nrow = num_var,
                ncol = n_obs[i])
    # The hiddens have sd sd_hiddens
    N[DAG_data$h, ] <- rnorm(n_obs[i] * length(DAG_data$h), sd = sd_hiddens)
    # The mean shift is applied
    N[DAG_data$x, ] <- N[DAG_data$x, ] + w[, i]
    # Here X denotes all the variables (both X's, H's, and Y).
    # It is the solution to the equation X = BX + N
    X <- solve(diag(num_var) - DAG_data$B) %*% N
    rownames(X) <- paste("X", 1:num_var, sep = "")
    X %>%
      t %>%
      as.matrix %>%
      as_tibble
  }
  
  # list where i'th entry is simulated data for i'th intervention
  sim_list <- lapply(1:num_interv, interv_sim)

  # Permutes the rows of the i'th intervention experiment
  if (n_obs_each == 2) {
    # If there are exactly two observations in each setting
    # then those two observations are flipped.
    permute_experiment <- function(i) sim_list[[i]][c(2,1), ]
  } else {
    # Otherwise the rows are randomly permuted.
    # Saves a permutation for each intervention experiment
    permutation_list <- lapply(n_obs, function(n) sample(n))
    permute_experiment <- function(i) sim_list[[i]][permutation_list[[i]], ]
  }
  # Applies all permutations to obtain tilde data sets
  sim_list_tilde <- lapply(1:num_interv, permute_experiment)
  # Collects all rows in one tibble
  sim_tib <- do.call(rbind, sim_list)
  sim_tib_tilde <- do.call(rbind, sim_list_tilde)
  
  # Returns relevant data
  DAG_data$dat <- list()
  DAG_data$dat$X <- as.matrix(sim_tib[, DAG_data$x])
  DAG_data$dat$Y <- as.matrix(sim_tib[, DAG_data$y])
  DAG_data$dat$X_tilde <-
    as.matrix(sim_tib_tilde[, DAG_data$x])
  DAG_data$dat$Y_tilde <-
    as.matrix(sim_tib_tilde[, DAG_data$y])
  DAG_data$dat$w <- w
  # We must save a list of environment IDs to use ICP
  DAG_data$dat$setting <- rep(1:num_interv, n_obs)

  return(DAG_data)
}

# Simulates alltargets data sets for each DAG in DAG_list
sim_alltargets_datasets <- function(DAG_list, n_obs_each, num_interv,
                                    shift_noise_sd, sd_hiddens,
                                    dir = "data/", max_MB_per_file = 500) {
    n_DAGs <- length(DAG_list)
    expected_MB_total <- 145 * (n_obs_each * num_interv) / 300 + 11
    expected_MB_per_DAG <- expected_MB_total / n_DAGs
    DAGs_per_file <- min(max(round(max_MB_per_file / expected_MB_per_DAG), 1),
                                n_DAGs)

    num_files_full <- floor(n_DAGs / DAGs_per_file)
    file_cut_points <- cumsum(c(0, rep(DAGs_per_file, num_files_full)))
    if (n_DAGs %% DAGs_per_file != 0) file_cut_points <-
                                c(file_cut_points, n_DAGs)

    setting_name <- sprintf("alltargets_%d_%d_sdw%d_sdh%d",
                        n_obs_each, num_interv, shift_noise_sd, sd_hiddens)

    dir_complete <- str_c(dir, setting_name)
    dir <- str_c(dir_complete, "_incomplete")

    file_index_start <- 1

    if (dir.exists(dir_complete))  {
        cat(sprintf("!!! WARNING: Dir %s already exists. Skipping this one. !!!\n", dir_complete))
        return(1)
    } else if (dir.exists(dir)) {
        # Finds highest occuring file
        str_extract(list.files(dir), "[0-9]+(?=.rds)") %>%
            na.omit %>%
            as.numeric %>%
            max(0) ->
            file_index_start
        # Sets file index to start at next file
        file_index_start <- file_index_start + 1
        cat(sprintf("!!! WARNING: Dir %s already exists !!!\n# Assuming %d MB per file | Continuing from file %d.rds\n\n",
                    dir, max_MB_per_file, file_index_start))
    } else {
        dir.create(dir)
    }

    num_files_total <- length(file_cut_points) - 1

    # Creates empty file which states the total number of files
    # there will be in the directory, when the function is done.
    # (This is to allow the user to remember later whether the function
    # was interrupted.)
    file.create(sprintf("%s/n_files_when_complete_%d", dir, num_files_total))

    cat(sprintf("Simulating %s\n", setting_name))
    cat(sprintf("%d files | %.1f MB each | %.1f GB total\n",
        num_files_total,
        expected_MB_total / num_files_total,
        expected_MB_total / 1000))

    for (file_index in file_index_start:num_files_total) {
        current_DAGs <- DAG_list[(file_cut_points[file_index] + 1):
                                file_cut_points[file_index + 1]]
        n_current_DAGs <- length(current_DAGs)
        cat(sprintf("\nFile %d / %d\n%d DAGs: ",
                file_index, num_files_total, n_current_DAGs))
        for (i in 1:n_current_DAGs) {
            cat(sprintf("%d|", i))
            current_DAGs[[i]] <- sim_alltargets(current_DAGs[[i]],
                                    n_obs_each = n_obs_each,
                                    num_interv = num_interv,
                                    shift_noise_sd = shift_noise_sd,
                                    sd_hiddens = sd_hiddens)
        }
        saveRDS(current_DAGs, sprintf("%s/%d.rds", dir, file_index))
        cat("Done!\n")
    }

    # Creates empty file "completed" in directory
    file.create(str_c(dir, "/completed"))

    # Renames directory
    system2("mv", args = c(dir, dir_complete))

}

# Simulates alltargets data sets for each DAG in DAG_list
# for each row in the params text file
# The column order is:  
# n_obs_each num_interv shift_noise_sd sd_hiddens
sim_alltargets_txt <- function(DAG_list,
                                txt_path,
                                dir = "data/",
                                max_MB_per_file = 500)
{
  mat <- matrix(scan(txt_path),
                ncol = 4, byrow = T)
  n_sims <- nrow(mat)

  for (i in 1:n_sims) {
    cat(sprintf("\n### Simulation setting %d / %d ###\n\n",
                  i, n_sims))
    sim_alltargets_datasets(
      DAG_list, mat[i, 1], mat[i, 2], mat[i, 3], mat[i, 4],
      dir = dir, max_MB_per_file = max_MB_per_file
    )
  }
}





# Simulates data from the singletargets setup.
# Input:
#   DAG_data: A DAG_data object
#   n_obs_each: Number of observations for each interventional setting
#   num_interv_each: Number of interventions to do for each of the
#                    X's intervened on.
#   num_x_interv: Number of X's to intervene on
#   n_obs_control: Number of observations with no intervention.
#   shift_noise_sd: sd of the mean shift.
#   sd_hiddens: sd of the hiddens.
sim_singletargets <- function(DAG_data,
                              n_obs_each,
                              num_interv_each,
                              num_x_interv,
                              n_obs_control,
                              shift_noise_sd,
                              sd_hiddens) {
  # Indices of X's intervened on.
  interv_indices <- sort(sample(DAG_data$x, num_x_interv))
  # Vector where the i'th entry is the index of the X intervened on
  # in the i'th environment
  interv <- rep(interv_indices, each = num_interv_each)
  # Shift noise where the i'th entry is the mean shift applied to
  # the X with index interv[i] in the i'th environment
  w <- rnorm(num_x_interv * num_interv_each, mean = 0, sd = shift_noise_sd)

  # Total number of different interventional environments
  # (this means excluding the control setting)
  num_interventional_settings <- num_interv_each * num_x_interv

  # The i'th entry is the number of observations of the i'th
  # interventional setting.
  n_obs <- rep(n_obs_each, times = num_interventional_settings)

  num_var <- nrow(DAG_data$B)

  # Simulates n_obs_each observations from the i'th interventional setting.
  interv_sim <- function(i) {
    # rows for variables, columns for repetitions.
    N <- matrix(rnorm(n_obs[i] * num_var),
                nrow = num_var,
                ncol = n_obs[i])
    # adds mean shift
    N[interv[i], ] <- N[interv[i], ] + rep(w[i], n_obs[i])
    # hiddens have sd sd_hiddens
    N[DAG_data$h, ] <- rnorm(n_obs[i] * length(DAG_data$h), sd = sd_hiddens)
    # X here means all variables (X, Y, and H)
    X <- solve(diag(num_var) - DAG_data$B) %*% N
    rownames(X) <- paste("X", 1:num_var, sep = "")
    X %>%
      t %>%
      as.matrix %>%
      as_tibble
  }

  # Simulates control data set. Only difference from the above is that
  # no mean shifts are added.
  N <- matrix(rnorm(n_obs_control * num_var),
                nrow = num_var,
                ncol = n_obs_control)
  N[DAG_data$h, ] <- rnorm(n_obs_control * length(DAG_data$h), sd = sd_hiddens)
  X <- solve(diag(num_var) - DAG_data$B) %*% N
  rownames(X) <- paste("X", 1:num_var, sep = "")
  X %>%
    t %>%
    as.matrix %>%
    as_tibble ->
    control_tib

  # list where i'th entry is simulated data for i'th intervention
  sim_list <- lapply(1:num_interventional_settings, interv_sim)
  # Functions to permute rows from i'th environment
  if (n_obs_each == 2) {
    # If there are exactly two observations in each setting
    # then those two observations are flipped.
    permute_experiment <- function(i) sim_list[[i]][c(2,1), ]
  } else {
    # Otherwise the rows are randomly permuted.
    # Saves a permutation for each intervention experiment
    permutation_list <- lapply(n_obs, function(n) sample(n))
    permute_experiment <- function(i) sim_list[[i]][permutation_list[[i]], ]
  }

  # Permutes control observations
  if (n_obs_control == 2) {
    control_tib_tilde <- control_tib[c(2,1), ]
  } else {
    control_tib_tilde <- control_tib[sample(n_obs_control), ]
  }

  # Applies all permutations to obtain tilde data sets
  sim_list_tilde <- lapply(1:num_interventional_settings, permute_experiment)
  # Collects all rows in one tibble
  sim_tib <- rbind(do.call(rbind, sim_list), control_tib)
  sim_tib_tilde <- rbind(do.call(rbind, sim_list_tilde), control_tib_tilde)

  # Returns relevant data
  DAG_data$dat <- list()
  DAG_data$dat$X <- as.matrix(sim_tib[, DAG_data$x])
  DAG_data$dat$Y <- as.matrix(sim_tib[, DAG_data$y])
  DAG_data$dat$X_tilde <- as.matrix(sim_tib_tilde[, DAG_data$x])
  DAG_data$dat$Y_tilde <- as.matrix(sim_tib_tilde[, DAG_data$y])
  DAG_data$dat$w <- w
  DAG_data$dat$setting <- c(
          rep(1:num_interventional_settings, each = n_obs_each), # intervention settings
          rep(0, n_obs_control) # control setting
  )
  # 0 indicates control data set.
  DAG_data$dat$interv <- c(rep(interv, each = n_obs_each),
                            rep(0, n_obs_control))

  return(DAG_data)
}

# Simulates singletargets data sets for each DAG in DAG_list
sim_singletargets_datasets <- function(DAG_list,
                                    n_obs_each,
                                    num_interv_each,
                                    num_x_interv,
                                    n_obs_control,
                                    shift_noise_sd,
                                    sd_hiddens,
                                    dir = "data/",
                                    max_MB_per_file = 500) {
    n_DAGs <- length(DAG_list)
    expected_MB_total <- 145 *
                (num_x_interv * n_obs_each * num_interv_each) /
                300 + 11
    expected_MB_per_DAG <- expected_MB_total / n_DAGs
    DAGs_per_file <- min(max(round(max_MB_per_file / expected_MB_per_DAG), 1),
                                n_DAGs)

    num_files_full <- floor(n_DAGs / DAGs_per_file)
    file_cut_points <- cumsum(c(0, rep(DAGs_per_file, num_files_full)))
    if (n_DAGs %% DAGs_per_file != 0) file_cut_points <-
                                c(file_cut_points, n_DAGs)

    setting_name <- sprintf("singletargets_%d_%d_%d_%d_sdw%d_sdh%d",
                        n_obs_each,
                        num_interv_each,
                        num_x_interv,
                        n_obs_control,
                        shift_noise_sd,
                        sd_hiddens)

    dir_complete <- str_c(dir, setting_name)
    dir <- str_c(dir_complete, "_incomplete")

    file_index_start <- 1

    if (dir.exists(dir_complete))  {
        cat(sprintf("!!! WARNING: Dir %s already exists. Skipping this one. !!!\n", dir_complete))
        return(1)
    } else if (dir.exists(dir)) {
        # Finds highest occuring file
        str_extract(list.files(dir), "[0-9]+(?=.rds)") %>%
            na.omit %>%
            as.numeric %>%
            max(0) ->
            file_index_start
        # Sets file index to start at next file
        file_index_start <- file_index_start + 1
        cat(sprintf("!!! WARNING: Dir %s already exists !!!\n# Assuming %d MB per file | Continuing from file %d.rds\n\n",
                    dir, max_MB_per_file, file_index_start))
    } else {
        dir.create(dir)
    }

    num_files_total <- length(file_cut_points) - 1

    # Creates empty file which states the total number of files
    # there will be in the directory, when the function is done.
    # (This is to allow the user to remember later whether the function
    # was interrupted.)
    file.create(sprintf("%s/n_files_when_complete_%d", dir, num_files_total))

    cat(sprintf("Simulating %s\n", setting_name))
    cat(sprintf("%d files | %.1f MB each | %.1f GB total\n",
        num_files_total,
        expected_MB_total / num_files_total,
        expected_MB_total / 1000))

    for (file_index in file_index_start:num_files_total) {
        current_DAGs <- DAG_list[(file_cut_points[file_index] + 1):
                                file_cut_points[file_index + 1]]
        n_current_DAGs <- length(current_DAGs)
        cat(sprintf("\nFile %d / %d\n%d DAGs: ",
                file_index, num_files_total, n_current_DAGs))
        for (i in 1:n_current_DAGs) {
            cat(sprintf("%d|", i))
            current_DAGs[[i]] <- sim_singletargets(current_DAGs[[i]],
                                    n_obs_each = n_obs_each,
                                    num_interv_each = num_interv_each,
                                    num_x_interv = num_x_interv,
                                    n_obs_control = n_obs_control,
                                    shift_noise_sd = shift_noise_sd,
                                    sd_hiddens = sd_hiddens)
        }
        saveRDS(current_DAGs, sprintf("%s/%d.rds", dir, file_index))
        cat("Done!\n")
    }

    # Creates empty file "completed" in directory
    file.create(str_c(dir, "/completed"))

    # Renames directory
    system2("mv", args = c(dir, dir_complete))

}

# Simulates alltargets data sets for each DAG in DAG_list
# for each row in the params text file
# The column order is:
# n_obs_each num_interv_each num_x_interv
# n_obs_control shift_noise_sd sd_hiddens
sim_singletargets_txt <- function(DAG_list,
                                txt_path,
                                dir = "data/",
                                max_MB_per_file = 500)
{
  mat <- matrix(scan(txt_path),
                ncol = 6, byrow = T)
  n_sims <- nrow(mat)

  for (i in 1:n_sims) {
    cat(sprintf("\n### Simulation setting %d / %d ###\n\n",
                  i, n_sims))
    sim_singletargets_datasets(
      DAG_list,
      mat[i, 1], mat[i, 2], mat[i, 3],
      mat[i, 4], mat[i, 5], mat[i, 6],
      dir = dir,
      max_MB_per_file = max_MB_per_file
    )
  }
}

