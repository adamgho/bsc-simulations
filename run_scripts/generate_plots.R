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

AUC_alltargets <- tibble(readRDS("data/AUC_alltargets.rds"))
AUC_alltargets_wide <- wide_AUC(AUC_alltargets)

### Singletargets

# alltargets, varying num_interv_each.
AUC_alltargets_wide %>% 
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

# alltargets, varying sdw
AUC_alltargets_wide %>% 
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

# alltargets, varying sdh
AUC_alltargets_wide %>% 
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


# ROC curve for an example

ROC_tib <- tibble(readRDS("data/alltargets_10_500_sdw7_sdh5/ROC_points.rds"))

ggplot(ROC_tib, aes(x = fpr_anc_mean, y = tpr_anc_mean, col = method)) +
    geom_point() +
    geom_line() +
    geom_abline(lty = 2)

tpr_fpr_tib <- tibble(readRDS("data/alltargets_10_500_sdw7_sdh5/tpr_fpr_DPOLS-coef.rds"))
length(unique(tpr_fpr_tib$DAG_id))
tpr_fpr_tib$DAG_id
ROC_tib <- tibble(readRDS("data/alltargets_2_20_sdw7_sdh5/ROC_points.rds"))

ggplot(ROC_tib, aes(x = fpr_anc_mean, y = tpr_anc_mean, col = method)) +
    geom_point() +
    geom_line() +
    geom_abline(lty = 2)

ROC_tib <- tibble(readRDS("data/alltargets_2_2000_15_100_sdw7_sdh5/ROC_points.rds"))

ggplot(ROC_tib, aes(x = fpr_anc_mean, y = tpr_anc_mean, col = method)) +
    geom_point() +
    geom_line() +
    geom_abline(lty = 2)
