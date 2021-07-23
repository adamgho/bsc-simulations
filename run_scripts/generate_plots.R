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
    strip.background = element_rect(fill = "transparent",colour = NA),
    legend.position = 'bottom'
  )
)

imw <- 14
imh <- 20

wide_AUC <- function(AUC_tib) {
    pivot_wider(AUC_tib, names_from = measure,
                names_prefix = "AUC_",
                values_from = AUC)  
}


### Permuted experiments

AUC_alltargets <- tibble(readRDS("data/small/AUC_alltargets.rds")) 
AUC_alltargets_wide <- wide_AUC(AUC_alltargets)

AUC_singletargets <- tibble(readRDS("data/small/AUC_singletargets.rds"))
AUC_singletargets_wide <- wide_AUC(AUC_singletargets)

### Singletargets

# singletargets_2_X_Y_2X_sdw7_sdh5 where Y in {15, 30}

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
Number of control observations: 2x.
Shift noise sd: 7.
Hiddens sd: 5"
)

ggsave('figures/singletargets_2_x_y_2x_sdw7_sdh5.pdf',
       width = imw,
       height = imh,
       units = 'cm')


# singletargets_10_X_Y_10X_sdw7_sdh5 where Y in {15, 30}

AUC_singletargets_wide %>% 
    filter(num_x_interv %in% c(15, 30),
           shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each == 10,
            n_obs_control == 10 * num_interv_each) %>% 
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
Number of control observations: 10x.
Shift noise sd: 7.
Hiddens sd: 5"
)

ggsave('figures/singletargets_10_x_y_10x_sdw7_sdh5.pdf',
       width = imw,
       height = imh,
       units = 'cm')



### alltargets

## alltargets_y_x_sdw7_sdh5
AUC_alltargets_wide %>% 
    filter(shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = AUC_quartile1,
                    ymax = AUC_quartile3,
                    fill = method),
                alpha = 0.1,
                col = 0) +
    facet_grid(n_obs_each ~ type) +
       labs(
        x = "Number of environments",
        y = "Average AUC",
        title =
"
alltargets.
# obs per environment: 2 resp. 10.
Shift noise sd: 7.
Hiddens sd: 5"
)

ggsave('figures/alltargets_y_x_sdw7_sdh5.pdf',
       width = imw,
       height = imh,
       units = 'cm')



## alltargets_x_500_sdw7_sdh5
AUC_alltargets_wide %>% 
    filter(shift_noise_sd == 7,
            sd_hiddens == 5,
           num_interv == 500) %>% 
    ggplot(aes(x = n_obs_each, y = AUC_mean, col = method)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ type) +
       labs(
        x = "Number of observations per environment",
        y = "Average AUC",
        title =
"
alltargets.
Number of environments: 500.
Shift noise sd: 7.
Hiddens sd: 5"
)

ggsave('figures/alltargets_x_500_sdw7_sdh5.pdf',
       width = imw,
       height = imh,
       units = 'cm')


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
    facet_wrap(~ type) +
       labs(
        x = "#environments/#obsservations per environment",
        y = "Average AUC",
        title =
"
alltargets.
Total number of observations: 5000.
Shift noise sd: 7.
Hiddens sd: 5."
)

ggsave('figures/alltargets_5000obs.pdf',
       width = imw,
       height = imh,
       units = 'cm')



# alltargets_10_500_sdwx_sdh5
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
    facet_wrap(~ type) +
       labs(
        x = "Shift noise sd",
        y = "Average AUC",
        title =
"
alltargets.
#observations per environment: 10
#environments: 500
Hiddens sd: 5."
)

ggsave('figures/alltargets_10_500_sdwx_sdh5.pdf',
       width = imw,
       height = imh,
       units = 'cm')


# alltargets_10_500_sdw7_sdhx
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
    facet_wrap(~ type) +
           labs(
        x = "Hiddens sd",
        y = "Average AUC",
        title =
"
alltargets.
#observations per environment: 10
#environments: 500
Shift noise sd: 7."
)

ggsave('figures/alltargets_10_500_sdw7_sdhx.pdf',
       width = imw,
       height = imh,
       units = 'cm')


# alltargets_2_2500_sdwx_sdh5

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
    facet_wrap(~ type) +
       labs(
        x = "Shift noise sd",
        y = "Average AUC",
        title =
"
alltargets.
#observations per environment: 2
#environments: 2500
Hiddens sd: 5."
)

ggsave('figures/alltargets_2_2500_sdwx_sdh5.pdf',
       width = imw,
       height = imh,
       units = 'cm')


# alltargets_2_2500_sdw7_sdhx

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
    facet_wrap(~ type) +
       labs(
        x = "Hiddens sd",
        y = "Average AUC",
        title =
"
alltargets.
#observations per environment: 2
#environments: 2500
Shift noise sd: 7."
)

ggsave('figures/alltargets_2_2500_sdw7_sdhx.pdf',
       width = imw,
       height = imh,
       units = 'cm')


### Separate experiments

AUC_alltargets_sep <- tibble(readRDS("data/separate/AUC_alltargets.rds")) 
AUC_alltargets_sep_wide <- wide_AUC(AUC_alltargets_sep)

AUC_singletargets_sep <- tibble(readRDS("data/separate/AUC_singletargets.rds"))
AUC_singletargets_sep_wide <- wide_AUC(AUC_singletargets_sep)

## alltargets_10_x_sdw7_sdh5
AUC_alltargets_sep_wide %>% 
    filter(shift_noise_sd == 7,
            sd_hiddens == 5,
            n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_line() +
    geom_point() +
    facet_grid(n_obs_each ~ type) +
       labs(
        x = "Number of environments",
        y = "Average AUC",
        title =
"
alltargets.
# obs per environment: 10.
Shift noise sd: 7.
Hiddens sd: 5"
)

# ROC curve for an example


get_ROC_curves <- function(setting_name, id) {
    tibble(readRDS(str_c('data/small/', setting_name, '/tpr_fpr.rds'))) %>%
        filter(DAG_id == id) ->
        tpr_fpr_tib
    
    list(ggplot(tpr_fpr_tib, aes(x = fpr_anc, y = tpr_anc, col = method)) +
         geom_point() +
         geom_line() +
         geom_abline(lty = 2) +
         facet_wrap(~ method),
         ggplot(tpr_fpr_tib, aes(x = fpr_pa, y = tpr_pa, col = method)) +
         geom_point() +
         geom_line() +
         geom_abline(lty = 2) +
         facet_wrap(~ method)
    )
}

rc <- get_ROC_curves('alltargets_10_500_sdw7_sdh5', 3)
rc[1]
rc[2]

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


AUC_alltargets_wide %>%
    filter(n_obs_each == 10,
           num_interv == 500,
           method == 'randomguess')

