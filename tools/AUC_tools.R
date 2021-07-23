suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(stringr)))
suppressWarnings(suppressMessages(library(tidyr)))

source("tools/methods.R")

## The default directory to use is contained in the file
## run_scripts/data_dir.txt
default_dir <- scan('run_scripts/data_dir.txt',
                    what = 'char',
                    quiet = TRUE)

# Vector of how many x's to select when getting ROC points
n_selects <- 0:30
n_n_selects <- length(n_selects)

# Input:
#   anc_selected: The nodes selected as possible parents or ancestors.
#   DAG_data: A DAG_data object.
# Outputs:
#   Vector of 4 elements:
#     Element 1-2: True positive and false positive rate for anc_selected
#      when considered as an estimate of ancestors, meaning
#      $\mathrm{FPR} = \frac{\#\text{non-ancestors in anc_selected}}
#      {\#\text{non-ancestors}}$ and
#      $\mathrm{TPR} = \frac{\#\text{ancestors in anc_selected}}
#      {\#\text{ancestors}}.$
#     Element 3-4: True positive and false positive rate for anc_selected
#     when considered as an estimate of parents, meaning
#      $\mathrm{FPR} = \frac{\#\text{non-parents in anc_selected}}
#      {\#\text{non-parents}}$ and
#      $\mathrm{TPR} = \frac{\#\text{parents in anc_selected}}
#      {\#\text{parents}}.$ 
tpr_fpr_vec <- function(anc_selected, DAG_data) {
    c(
        get_tpr_fpr(anc_selected, DAG_data$anc_y, DAG_data$x),
        get_tpr_fpr(anc_selected, DAG_data$pa_y, DAG_data$x)
    )
}

# anc_selected and anc_y could also be parents
# Input:
#   anc_selected: Selected ancestors, or selected parents.
#   anc_y: ancestors of y, or parents of y.
# Output:
#   Vector of true positive and false positive rate.
#   See comment for tpr_fpr_vec for details.
#   Passing pa_y gives element 3-4 of tpr_fpr_vec.
#   Passing anc_y gives element 1-2 of tpr_fpr_vec.
get_tpr_fpr <- function(anc_selected, anc_y, x) {
    # Ancestors of y in x
    anc_in_x <- intersect(anc_y, x)
    # x's that are not ancestors of y
    non_anc <- setdiff(x, anc_y)
    # Number of selected ancestors that are truly ancestors
    true_pos <- intersect(anc_selected, anc_y)
    # Number of selected ancestors that are actually not ancestors
    false_pos <- intersect(non_anc, anc_selected)
    # Calculates true positive rate.
    # If there are no ancestors of y in x, then the true positive rate is
    # set to 1
    tpr <- ifelse(length(anc_in_x) == 0,
                    1,
                    length(true_pos) / length(anc_in_x))
    # Calculates false positive rate.
    # If there are no x's that are not ancestors of y, then the false positive
    # rate is set to 0
    fpr <- ifelse(length(non_anc) == 0,
                    0,
                    length(false_pos) / length(non_anc))
    # Returns vector containing true positive and false positive rate.
    c(tpr, fpr)
}

# Gives all sim directories in dir
# Input:
#   sim_type: Either "alltargets" or "singletargets"
#   dir: Directory containing sim-directories
# Output:
#   Vector of all names of simulation directories in dir.
get_sim_dirs <- function(sim_type, dir = default_dir) {
    if (sim_type == "alltargets") {
        list.files(dir,
            pattern = str_c("^alltargets_[0-9]+_[0-9]+_sdw[0-9]+_sdh[0-9]+$")
        )
    } else if (sim_type == "singletargets") {
        list.files(dir,
            pattern = str_c("^singletargets_[0-9]+_[0-9]+_[0-9]+_[0-9]+_sdw[0-9]+_sdh[0-9]+$")
        )
    } else {
        return(NULL)
    }
}

# Saves true positive and false positive rates 
# Input:
#   dir: directory of data from a single simulation setup.
#   order_func: A function taking DAG_data$dat and returning
#               a vector with a value for each x, where a larger absolute
#               value means more significant.
#               OBS: If using p-values, you need to make order_func
#               return 1 - p_value (so large means significant).
#               (See one_minus below.)
#   method_name: String to include in tibble in "method" column.
#   n_DAGs_to_process: Number of DAGs to process.
#                      Default is to process all DAGs.
save_tpr_fpr <- function(dir, order_func, method_name,
                        n_DAGs_to_process = Inf) {
    # If there are only 2 observations in some environment, then ICP can't run.
    if (method_name == "ICP" &
        # First checks whether n_obs_each == 2 (for either setup)
        (( str_extract(
            dir,
            # The number after the first underscore is
            # the number of observations per environment.
            "(?<=targets_)[0-9]+(?=_)"
        ) %>% as.numeric < 3) |
        # Now checks whether it is singletargets and has less than 3 control
        # observations.
            (str_extract(dir, "[a-z]+(?=targets)") == "single" &
            str_extract(dir,
                        "(?<=singletargets_[0-9]{1,10}_[0-9]{1,10}_[0-9]{1,10}_)[0-9]+(?=_)") %>% 
                as.numeric < 3)))
    {
                   cat(sprintf("# Skipping %s\n(ICP needs min. three obs. per environment)\n\n",
                        dir))
            return(1)
    }

    res_tib <- tibble()

    # Only files of the form [one or more numbers].rds
    filenames <- str_sort(list.files(dir, pattern = "^[0-9]+.rds$"),
        numeric = T
    )

    if (n_DAGs_to_process == Inf) {
        cat(sprintf("### %s | %d files | %s | will process all DAGs ###\n",
                dir, length(filenames), method_name))
    } else {
    cat(sprintf("### %s | %d files | %s | will process %d DAGs ###\n", 
                dir, length(filenames), method_name, n_DAGs_to_process))
    }

    # Counter to keep track of when enough DAGs have been processed.
    n_DAGs_processed <- 0
    
    # Loops through files and processes DAGs until n_DAGs_to_process
    # have been processed. It then breaks out of the for loop.
    for (filename in filenames) {
        # breaks when the right amount of DAGs have been processed.
        # (It will be exactly the right amount due to the second break
        # statement further down).
        if (n_DAGs_processed == n_DAGs_to_process) break
        cat(sprintf("\nFile: %s\nDAG: ", filename))
        DAG_list <- readRDS(str_c(dir, "/", filename))
        n_DAGs <- length(DAG_list)
        res_mat <- matrix(NA,
            ncol = 4,
            # If there are fewer DAGs left to process than there are in the file
            # then we will break out of the loop after the correct amount,
            # so the matrix should have fewer rows than DAGs in the file.
            nrow = min(n_DAGs, n_DAGs_to_process - n_DAGs_processed) *
                        n_n_selects,
            dimnames = list(
                NULL,
                c(
                    "tpr_anc", "fpr_anc",
                    "tpr_pa", "fpr_pa"
                ) 
            )
        )
        row_start <- 1
        # Loops through DAGs in the file.
        # Breaks out of loop if it reaches the desired number of DAGs.
        for (i in 1:n_DAGs) {
            cat(sprintf("%d|", i))
            order_vals <- order_func(DAG_list[[i]]$dat)
            ## Returns X indices ordered by largest absolute "order" value.
            ## Breaks ties randomly
            # rev reverses the vector 
            x_order <- rev(DAG_list[[i]]$x[
                # order returns vector where the i'th entry is the
                # i'th smallest value in order_vals
                order(
                    # rank with random tie breaking to avoid ties.
                    # If just using order_vals (without rank first)
                    # order wouldn't break ties randomly.
                    rank(abs(order_vals), ties.method = "random")
                )
            ])

            # Selects the first n_select x's in x_order (the n_select most
            # significant x's) and saves results in res_mat.
            for (n_select in n_selects) {
                if (n_select == 0) selected <- NULL
                else selected <- x_order[1:n_select]
                tpr_fpr_vec(selected, DAG_list[[i]]) %>% 
                    unlist %>% 
                    matrix(nrow = 1,
                        byrow = T,
                        dimnames = list(method_name,
                                        c("tpr_anc", "fpr_anc",
                                            "tpr_pa", "fpr_pa"))) ->
                    res_mat[row_start, ]
                
                row_start <- row_start + 1
            }

            # Breaks if the right amount of DAGs have been processed.
            if (n_DAGs_processed + i == n_DAGs_to_process) break
        }

        # Formats results for this file with method name and DAG_ids and
        # adds to res_tib
        res_mat %>%
            as_tibble() %>%
            cbind(
                expand.grid(list(
                    method = method_name,
                    n_select = n_selects,
                    DAG_id = (n_DAGs_processed + 1):
                                (n_DAGs_processed + i)
                ))) %>% 
            rbind(res_tib) ->
            res_tib
        n_DAGs_processed <- n_DAGs_processed + i
        cat("Done!\n")
    }

    # Checks whether the total number of DAGs processed matches the n_DAGs_to_process.
    if (n_DAGs_to_process == Inf | n_DAGs_processed == n_DAGs_to_process) {
        cat(sprintf(
            "\n## Done processing all %d DAGs to process ##\n",
            n_DAGs_processed 
        ))
    } else {
        cat(sprintf(
            "\n!!! WARNING: Only %d DAGs in %s. You asked me to process %d DAGs. !!!\n",
            n_DAGs_processed, dir, n_DAGs_to_process
        ))
    }
    
    saveRDS(res_tib, str_c(dir, "/tpr_fpr_", method_name, ".rds"))
}



# Takes pval_func (a function returning p-values from dat) and
# returns a function giving 1 - p_values instead.
one_minus <- function(pval_func) {
    function(dat) 1 - pval_func(dat)
}

# Runs save_tpr_fpr on all data-directories in dir that don't already
# have a file named tpr_fpr_"method_name".rds
add_missing_tpr_fpr <- function(order_func, method_name,
                                sim_type = "alltargets",
                                n_DAGs_to_process = Inf,
                                dir = default_dir) {
    sim_dirs <- get_sim_dirs(sim_type, dir)

    for (sim_dir in sim_dirs) {
        complete_dir <- str_c(dir, "/", sim_dir)
        if (!file.exists(str_c(complete_dir, "/tpr_fpr_",
                                method_name, ".rds"))) {
            save_tpr_fpr(complete_dir,
                            order_func,
                            method_name,
                            n_DAGs_to_process)
        }
    }
}

## Gives first quartile
quartile1 <- function(x) quantile(x, 0.25)
## Gives third quartile
quartile3 <- function(x) quantile(x, 0.75)

save_AUC <- function(dir) {
    cat(sprintf("\n### AUC: %s ###\n", dir))

    ## Collects all tpr_fpr_method.rds tibbles in one tibble tpr_fpr.rds
    tib <- tibble()
    tpr_fpr_files <- list.files(dir, pattern = "tpr_fpr_")
    for (tpr_fpr_file in tpr_fpr_files) {
        tib <- rbind(tib, readRDS(str_c(dir, "/", tpr_fpr_file)))
    }
    ## If dir doesn't contain any rds files starting with 'tpr_fpr_' return 1
    ## and exit.
    if (nrow(tib) == 0) {
        cat("No tpr_fpr files. Skipping.\n")
        return(1)
    }
    saveRDS(tib, str_c(dir, "/tpr_fpr.rds"))

    ## Computes AUC for each DAG
    tib %>%
        group_by(DAG_id, method) %>%
        summarise(AUC_anc = get_AUC(tpr_anc, fpr_anc),
                  AUC_pa = get_AUC(tpr_pa, fpr_pa),
                  .groups = 'drop') ->
        all_AUC
    saveRDS(all_AUC, str_c(dir, '/all_AUC.rds'))

    ## Takes average of all AUC (across all DAGs)
    all_AUC %>%
        group_by(method) %>%
        summarise(pa_mean = mean(AUC_pa),
                  anc_mean = mean(AUC_anc),
                  pa_median = median(AUC_pa),
                  anc_median = median(AUC_anc),
                  pa_quartile3 = quartile3(AUC_pa),
                  anc_quartile3 = quartile3(AUC_anc),
                  pa_quartile1 = quartile1(AUC_pa),
                  anc_quartile1 = quartile1(AUC_anc),                  
                  .groups = 'drop') %>%
        pivot_longer(cols = -1,
                     names_to = c('type', 'measure'),
                     names_sep = '_',
                     values_to = 'AUC') %>%
        saveRDS(str_c(dir, '/AUC.rds'))
}

## Runs save_AUC on all data directories in dir.
add_missing_AUC <- function(sim_type = "alltargets", dir = default_dir) {
    sim_dirs <- get_sim_dirs(sim_type, dir)

    for (sim_dir in sim_dirs) {
        save_AUC(str_c(dir, "/", sim_dir))
    }
}

# Gives area under the ROC curve given by the points (fpr_i, tpr_i)
get_AUC <- function(tpr, fpr) {
    # First the area between x-axis and ROC curve is found.
    # Area of the first triangle.
    area <- tpr[1] * fpr[1] / 2
    # Add areas of each trapezoid.
    for (i in 2:length(tpr)) {
        area <- area +
            (fpr[i] - fpr[i - 1]) *
                (tpr[i] + tpr[i - 1]) / 2
    }
    return(area)
}

# Wrapper that calls the relevant version of collect_AUC for the sim_type.
collect_AUC <- function(sim_type = "alltargets", dir = default_dir) {
    do.call(str_c("collect_AUC_", sim_type), list(dir = dir))
}

# Collects all tibbles saved in files AUC.rds from alltargets directories in 
# dir in a single tibble and saves it in dir/AUC_alltargets.rds.
# The parameters for the different alltargets setups are saved in the tibble
# as well.
collect_AUC_alltargets <- function(dir = default_dir) {
    cat(sprintf("\n### Collecting alltargets AUC from %s ###\n", dir))

    sim_dirs <- get_sim_dirs("alltargets", dir)
    complete_tib <- tibble()
    
    # Loops through alltargets directories
    for (sim_dir in sim_dirs) {
        cat(sprintf("# %-25s", sim_dir))
        complete_dir <- str_c(dir, "/", sim_dir)
        AUC_path <- str_c(complete_dir, "/AUC.rds")
        if (file.exists(AUC_path)) {
            # Extracts n_obs_each, num_interv, shift_noise_sd, and sd_hiddens 
            # from the name of the directory.
            sim_dir %>% 
                str_extract_all("(?<=(_|sdw|sdh))[0-9]+(?=(_|$))",
                                simplify = T) %>% 
                as.numeric ->
                sim_info

            # Adds the data to complete_tib with columns giving
            # n_obs_each, num_interv and shift_noise_sd.
            readRDS(AUC_path) %>% 
                mutate(n_obs_each = sim_info[1],
                        num_interv = sim_info[2],
                        shift_noise_sd = sim_info[3],
                        sd_hiddens = sim_info[4]) %>% 
                        rbind(complete_tib) ->
                        complete_tib

            cat("Added\n")
        } else {
            # If sim_dir doesn't contain a file named AUC.rds
            # it is skipped
            cat("No AUC.rds\n")
        }
    }
    # Saves results
    saveRDS(complete_tib, str_c(dir, "/AUC_alltargets.rds"))
}


# Collects all AUC from singletargets directories in dir in
# a single tibble and saves it in dir/AUC_singletargets.rds.
# Similar to collect_AUC_alltargets (but for singletargets instead).
collect_AUC_singletargets <- function(dir = default_dir) {
    cat(sprintf("\n### Collecting AUC from %s ###\n", dir))

    sim_dirs <- get_sim_dirs("singletargets", dir)
    complete_tib <- tibble()

    for (sim_dir in sim_dirs) {
        cat(sprintf("# %-25s", sim_dir))
        complete_dir <- str_c(dir, "/", sim_dir)
        AUC_path <- str_c(complete_dir, "/AUC.rds")
        if (file.exists(AUC_path)) {
            # Extracts n_obs_each, num_interv, shift_noise_sd, and sd_hiddens 
            # from the name of the directory.
            sim_dir %>% 
                str_extract_all("(?<=(_|sdw|sdh))[0-9]+(?=(_|$))",
                                simplify = T) %>% 
                as.numeric ->
                sim_info

            # Adds the data to complete_tib with columns giving
            # n_obs_each, num_interv and shift_noise_sd.
            readRDS(AUC_path) %>% 
                mutate(n_obs_each = sim_info[1],
                        num_interv_each = sim_info[2],
                        num_x_interv = sim_info[3],
                        n_obs_control = sim_info[4],
                        shift_noise_sd = sim_info[5],
                        sd_hiddens = sim_info[6]) %>% 
                        rbind(complete_tib) ->
                        complete_tib

            cat("Added\n")
        } else {
            cat("No AUC.rds\n")
        }
    }
    
    saveRDS(complete_tib, str_c(dir, "/AUC_singletargets.rds"))
}
