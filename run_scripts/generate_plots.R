# Saves plots

library(tidyverse)
theme_set(
  theme_bw(base_size=12, base_family="Palatino") %+replace%
  theme(
    #panel.grid = element_blank(),
    axis.ticks = element_line(size = 0.3),
    #panel.border = element_blank(),
    axis.line = element_line(size = 0.3),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key        = element_rect(fill="transparent", colour=NA),
    strip.background = element_rect(fill = "transparent",colour = NA)
  )
)

wide_AUC <- function(AUC_tib) {
    pivot_wider(AUC_tib, names_from = measure,
                names_prefix = "AUC_",
                values_from = AUC)  
}


AUC_alltargets <- tibble(readRDS("data/AUC_alltargets.rds")) 
AUC_alltargets_wide <- wide_AUC(AUC_alltargets)

AUC_singletargets <- tibble(readRDS("data/AUC_singletargets.rds"))
AUC_singletargets_wide <- wide_AUC(AUC_singletargets)

### Singletargets

# singletargets, varying num_interv_each.
# 30 x interv, 100 obs control
AUC_singletargets_wide %>% 
    filter(n_obs_control == 100,
            num_x_interv == 30,
            shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 100.
# X's intervened on: 30.
Shift noise sd: 7.
Hiddens sd: 5"
    )

# singletargets, varying num_interv_each.
# 30 x interv, 10000 obs control
AUC_singletargets_wide %>% 
    filter(n_obs_control == 10000,
            num_x_interv == 30,
            shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2,10)) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 10000.
# X's intervened on: 30.
Shift noise sd: 7.
Hiddens sd: 5"
    )

# singletargets, varying num_interv_each.
# 15 x_interv, 100 obs control
AUC_singletargets_wide %>% 
    filter(n_obs_control == 100,
            num_x_interv == 15,
            shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 100.
# X's intervened on: 15.
Shift noise sd: 7.
Hiddens sd: 5"
    )

# singletargets, varying num_interv_each.
# 15 x interv, 10000 obs control
AUC_singletargets_wide %>% 
    filter(n_obs_control == 10000,
            num_x_interv == 15,
            shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2,10)) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 10000.
# X's intervened on: 15.
Shift noise sd: 7.
Hiddens sd: 5"
    )


# singletargets_2_X_Y_X_sdw7_sdh5 where Y in {15, 30}

AUC_singletargets_wide %>% 
    filter(num_x_interv %in% c(15, 30),
           shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each == 2,
            n_obs_control == 2 * num_interv_each) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(num_x_interv ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 2x.
Shift noise sd: 7.
Hiddens sd: 5"
)

                                        # singletagets_10_?_15_100_sdw7_sdh5 to compare ICP
AUC_singletargets_wide %>% 
    filter(num_x_interv == 15,
           shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each == 10,
           n_obs_control == 100,
           num_interv_each <= 500) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(num_x_interv ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 2x.
Shift noise sd: 7.
Hiddens sd: 5"
)

                                        # singletagets_10_?_30_100_sdw7_sdh5 to compare ICP
AUC_singletargets_wide %>% 
    filter(num_x_interv == 30,
           shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each == 10,
           n_obs_control == 100,
           num_interv_each <= 500) %>% 
    ggplot(aes(x = num_interv_each,
                y = AUC_mean,
                col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(num_x_interv ~ type) +
    labs(
        x = "Number of interventions per X intervened on.",
        y = "Average AUC",
        title =
"
Singletargets.
Control obs: 2x.
Shift noise sd: 7.
Hiddens sd: 5"
)


### alltargets

                                        # alltargets, varying num_interv.
AUC_alltargets_wide %>% 
    filter(shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type)


                                        # Compare with ICP for alltargets_10_X
AUC_alltargets_wide %>% 
    filter(shift_noise_sd == 7,
            sd_hiddens == 5,
           n_obs_each == 10,
           num_interv <= 10000) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type)


                                        # alltargets, varying n_obs_each.
AUC_alltargets_wide %>% 
    filter(shift_noise_sd == 7,
            sd_hiddens == 5,
            num_interv == 500) %>% 
    ggplot(aes(x = n_obs_each, y = AUC_mean, col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type)

# alltargets, 5000 obs, varying both n_obs_each and num_interv
AUC_alltargets_wide %>% 
    filter(
        n_obs_each * num_interv == 5000,
        shift_noise_sd == 7,
        sd_hiddens == 5
    ) %>% 
    ggplot(aes(x = num_interv / n_obs_each,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

# alltargets, varying sdw
AUC_alltargets_wide %>% 
    filter(
        n_obs_each == 10,
        num_interv == 500,
        sd_hiddens == 5
    ) %>% 
    ggplot(aes(x = shift_noise_sd,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)


# alltargets, varying sdh
AUC_alltargets_wide %>% 
    filter(
        n_obs_each == 10,
        num_interv == 500,
        shift_noise_sd == 7
    ) %>% 
    ggplot(aes(x = sd_hiddens,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

# alltargets_2, varying sdw
AUC_alltargets_wide %>% 
    filter(
        n_obs_each == 2,
        num_interv == 2500,
        sd_hiddens == 5
    ) %>% 
    ggplot(aes(x = shift_noise_sd,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)


# alltargets_2, varying sdh
AUC_alltargets_wide %>% 
    filter(
        n_obs_each == 10,
        num_interv == 500,
        shift_noise_sd == 7
    ) %>% 
    ggplot(aes(x = sd_hiddens,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

# ROC curve for an example

get_ROC_curves <- function(setting_name) {
    ROC_tib <- tibble(readRDS(str_c('data/', setting_name, '/ROC_points.rds')))

    list(ggplot(ROC_tib, aes(x = fpr_anc_mean, y = tpr_anc_mean, col = method)) +
        geom_point() +
        geom_line() +
        geom_abline(lty = 2),
    ggplot(ROC_tib, aes(x = fpr_pa_mean, y = tpr_pa_mean, col = method)) +
        geom_point() +
        geom_line() +
        geom_abline(lty = 2))
}

# ROC curve to compare with ICP for alltargets_200_500
icp <- get_ROC_curves('alltargets_200_500_sdw7_sdh5')
icp[1]
icp[2]

### Not to be included
                                        # singletargets, varying sdw
AUC_singletargets_wide %>% 
    filter(
        n_obs_each == 2,
        num_interv_each == 2000,
        num_x_interv == 15,
        n_obs_control == 100,
        sd_hiddens == 5
    ) %>% 
    ggplot(aes(x = shift_noise_sd,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

                                        # singletargets, varying sdh
AUC_singletargets_wide %>% 
    filter(
        n_obs_each == 2,
        num_interv_each == 2000,
        num_x_interv == 15,
        n_obs_control == 100,
        shift_noise_sd == 7
    ) %>% 
    ggplot(aes(x = sd_hiddens,
                y = AUC_mean,
                col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)
