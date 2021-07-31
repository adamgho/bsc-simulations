# Saves plots

library(tidyverse)
theme_set(
    theme_bw(base_size=12, base_family="") %+replace%
    theme(
        ##panel.grid = element_blank(),
        axis.ticks = element_line(size = 0.3),
        ##panel.border = element_blank(),
        axis.line = element_line(size = 0.3),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA),
        strip.background = element_rect(fill = "transparent",colour = NA),
        legend.position = 'bottom',
        legend.box = 'vertical',
        plot.title = element_text(hjust = 0.5)
    )
)

library(tikzDevice)

imw <- 6
imh <- 7

## Converts AUC_tib to a wider format with a column for each of AUC_mean, AUC_median,
## AUC_quartile3, and AUC_quartile4.
wide_AUC <- function(AUC_tib) {
    pivot_wider(AUC_tib, names_from = measure,
                names_prefix = "AUC_",
                values_from = AUC)  
}

## Reads tibble in wide format from rds file.
read_wide <- function(file_name) {
    wide_AUC(tibble(readRDS(file_name)))
}

## Renames pa to Parents and anc to Ancestors

nicer_type <- function(type) {
    ifelse(type == 'anc', 'Ancestors', 'Parents')
}

## Reads randomguess performance

random30 <- read.table('local_results/nx30_AUC_random_summary.txt',
                       header = TRUE)

random30 %>%
    pivot_longer(cols = everything(),
                 names_to = c('type', 'measure'),
                 names_prefix = 'AUC_',
                 names_sep = '_') %>%
    pivot_wider(names_from = measure,
                names_prefix = 'AUC_',
                values_from = value) %>%
    mutate(type = nicer_type(type)) ->
    random30

## I create the line as a ribbon with same upper and lower limit, since an hline
## extends further along the x-axis than a ribbon.
random30_lines <- list(
    geom_hline(data = random30,
               aes(yintercept = AUC_mean,
                   lty = 'random-mean'),
                size = 0.4,
               col = 1),
    geom_hline(data = random30,
               aes(yintercept = AUC_quartile3,
                   lty = 'random-quartile3'),
               size = 0.4,
               col = 1),
    scale_linetype_manual(name = NULL,
                            values = c('dashed', 'dotted'))
)


####### Permuting (non-separate)

## alltargets AUC
stor1_all <- read_wide('local_results/stor1/AUC_alltargets.rds')
stor2_all <- read_wide('local_results/stor2/AUC_alltargets.rds')
stor4_all <- read_wide('local_results/stor4/AUC_alltargets.rds')

## TODO: include stor4_all
AUC_all <- filter(rbind(stor1_all, stor2_all, stor4_all),
                  method != 'randomguess')
AUC_all$type <- nicer_type(AUC_all$type)

## singletargets AUC
stor1_single <- read_wide('local_results/stor1/AUC_singletargets.rds')
stor2_single <- read_wide('local_results/stor2/AUC_singletargets.rds')

## TODO: include stor4_single
AUC_single <- filter(rbind(stor1_single, stor2_single, stor4_single),
                     method != 'randomguess')
AUC_single$type <- nicer_type(AUC_single$type)

main_methods <- c('OLS-coef', 'OLS-pvals',
                  'POLS-coef', 'POLS-pvals',
                  'DPOLS-coef', 'DPOLS-pvals')

col_main_methods <- c('#44bc44', '#a9d566',
                      '#2fafff', '#97d7ff',
                      '#ff8059', '#ffbfac')

scale_main <- scale_color_manual(
    breaks = main_methods,
    values = col_main_methods)

mean_shift <- 'mean-shift'
col_mean_shift <- 'yellow2'

scale_mean <- scale_color_manual(
    breaks = c(main_methods, mean_shift),
    values = c(col_main_methods, col_mean_shift))

ICPs <- c('ICP', 'PICP')
col_ICPs <- c('#feacd0', '#b6a0ff')

scale_ICPs <- scale_color_manual(
    breaks = c(main_methods, ICPs),
    values = c(col_main_methods, col_ICPs))

scale_all <- scale_color_manual(
    breaks = c(main_methods, ICPs, mean_shift),
    values = c(col_main_methods, col_ICPs, col_mean_shift))

##### 30 X and 30 H

label_obs <- function(n_obs_each) {
    paste(n_obs_each, 'obs. per env.')
}

tikz(file = '~/thesis/figures/alltargets_vary_num_interv.tex', width=imw, height=imh)
AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point(size = 0.8) +
    facet_grid(n_obs_each ~ type,
               labeller = labeller(n_obs_each = label_obs)) +
    scale_main +
    labs(
        x = "Number of environments",
        y = "Average AUC",
        title =
            "alltargets -- varying number of environments"
    )
endoffile <- dev.off()

tikz(file = '~/thesis/figures/alltargets_x_500_7_5.tex', width=imw, height=imh)
AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           num_interv == 500) %>% 
    ggplot(aes(x = n_obs_each, y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point() +
    facet_wrap( ~ type, nrow = 2) +
    scale_main +
    labs(
        x = "Number of observations per environment",
        y = "Average AUC",
        title =
            "alltargets -- varying number of observations per environment"
    )
endoffile <- dev.off()

tikz(file = '~/thesis/figures/alltargets_5000obs.tex', width=imw, height=imh)
AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           num_interv*n_obs_each == 5000) %>% 
    ggplot(aes(x = log(num_interv / n_obs_each), y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point() +
    scale_main +
    facet_wrap( ~ type, nrow = 2) +
       labs(
           x = "$\\log \\frac{\\# \\mathrm{environments}}{\\# \\mathrm{observations\\ per\\ environment}}$",
           y = "Average AUC",
           title =
               "alltargets -- few env. with many obs. vs. many env. with few obs."
       )
endoffile <- dev.off()

tikz(file = '~/thesis/figures/alltargets_vary_sdw.tex', width=imw, height=imh)
AUC_all %>% 
    filter(sd_hiddens == 5,
           (num_interv == 500 & n_obs_each == 10) |
           (num_interv == 2500 & n_obs_each == n_obs_each)) %>% 
    ggplot(aes(x = shift_noise_sd, y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point() +
    scale_main +
    facet_grid(n_obs_each ~ type,
               labeller = labeller(n_obs_each = label_obs)) +
       labs(
           x = "Standard deviation of mean shifts",
           y = "Average AUC",
           title =
               "alltargets -- varying standard deviation of mean shifts $W$"
       )
endoffile <- dev.off()

tikz(file = '~/thesis/figures/alltargets_vary_sdh.tex', width=imw, height=imh)
AUC_all %>% 
    filter(shift_noise_sd == 7,
           (num_interv == 500 & n_obs_each == 10) |
           (num_interv == 2500 & n_obs_each == 2)) %>% 
    ggplot(aes(x = sd_hiddens, y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point() +
    scale_main +
    facet_grid(n_obs_each ~ type,
               labeller = labeller(n_obs_each = label_obs)) +
       labs(
           x = "Standard deviation of hidden variables",
           y = "Average AUC",
           title =
               "alltargets -- varying standard deviation of hidden variables"
       )
endoffile <- dev.off()

## singletargets

label_num_x_interv <- function(num_x_interv) {
    sprintf("%s intervention targets", num_x_interv)
}

tikz(file = '~/thesis/figures/singletargets_2_x_y_2x_7_5.tex', width=imw, height=imh)
AUC_single %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each == 2,
           num_x_interv %in% c(15, 30),
           n_obs_control == n_obs_each * num_interv_each) %>% 
    ggplot(aes(x = num_interv_each, y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point() +
    scale_mean +
    facet_grid(num_x_interv ~ type,
               labeller = labeller(num_x_interv = label_num_x_interv)) +
       labs(
           x = "Number of repetitions of each experimental setting",
           y = "Average AUC",
           title =
               "singletargets -- varying number of repetitions per environment"
       )
endoffile <- dev.off()

tikz(file = '~/thesis/figures/singletargets_10_x_y_10x_7_5.tex', width=imw, height=imh)
AUC_single %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each == 10,
           num_x_interv %in% c(15, 30),
           n_obs_control == n_obs_each * num_interv_each) %>% 
    ggplot(aes(x = num_interv_each, y = AUC_mean, col = method)) +
    random30_lines +
    geom_line() +
    geom_point() +
    scale_mean +
    facet_grid(num_x_interv ~ type,
               labeller = labeller(num_x_interv = label_num_x_interv)) +
       labs(
           x = "Number of repetitions of each experimental setting",
           y = "Average AUC",
           title =
               "singletargets -- varying number of repetitions per environment"
       )
endoffile <- dev.off()

### End of implemented

stopifnot('Done executing implemented plots' = FALSE)

##### 5 X and 5 H

AUC_alltargets <- tibble(readRDS("local_results/nx5/AUC_alltargets.rds")) 
AUC_alltargets_wide <- wide_AUC(AUC_alltargets)

AUC_singletargets <- tibble(readRDS("local_results/nx5/AUC_singletargets.rds"))
AUC_singletargets_wide <- wide_AUC(AUC_singletargets)

## alltargets_10_x_sdw7_sdh5

AUC_alltargets_wide %>%
    filter(n_obs_each == 10,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## same as above, but only small values of num_interv


AUC_alltargets_wide %>%
    filter(n_obs_each == 10,
           num_interv <= 2500,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## alltargets_2_x_sdw7_sh5


AUC_alltargets_wide %>%
    filter(n_obs_each == 2,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## singletargets_10_x_5_10x_sdw7_sdh5

AUC_singletargets_wide %>%
    filter(n_obs_each == 10,
           num_x_interv == 5,
           n_obs_control == 10*num_interv_each,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv_each,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## singletargets_2_x_sdw7_sh5

AUC_singletargets_wide %>%
    filter(n_obs_each == 2,
           num_x_interv == 5,
           n_obs_control == 2*num_interv_each,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv_each,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

###### Separate experiments

##### 30 X and 30 H

## alltargets AUC
stor3_all <- tibble(readRDS('local_results/stor3/AUC_alltargets.rds'))
## stor5_all <- tibble(readRDS('local_results/stor5/AUC_alltargets.rds'))
stor6_all <- tibble(readRDS('local_results/stor6/AUC_alltargets.rds'))

## singletargets AUC
stor3_single <- tibble(readRDS('local_results/stor3/AUC_singletargets.rds'))
## stor5_single <- tibble(readRDS('local_results/stor5/AUC_singletargets.rds'))
stor6_single <- tibble(readRDS('local_results/stor6/AUC_singletargets.rds'))

##### 5 X and 5 H

## TODO: rename these (copy from permuted version)
AUC_alltargets <- tibble(readRDS("local_results/nx5/AUC_alltargets.rds")) 
AUC_alltargets_wide <- wide_AUC(AUC_alltargets)

AUC_singletargets <- tibble(readRDS("local_results/nx5/AUC_singletargets.rds"))
AUC_singletargets_wide <- wide_AUC(AUC_singletargets)

## alltargets_10_x_sdw7_sdh5

AUC_alltargets_wide %>%
    filter(n_obs_each == 10,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## same as above, but only small values of num_interv


AUC_alltargets_wide %>%
    filter(n_obs_each == 10,
           num_interv <= 2500,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## alltargets_2_x_sdw7_sh5


AUC_alltargets_wide %>%
    filter(n_obs_each == 2,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## singletargets_10_x_5_10x_sdw7_sdh5

AUC_singletargets_wide %>%
    filter(n_obs_each == 10,
           num_x_interv == 5,
           n_obs_control == 10*num_interv_each,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv_each,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)

## singletargets_2_x_sdw7_sh5

AUC_singletargets_wide %>%
    filter(n_obs_each == 2,
           num_x_interv == 5,
           n_obs_control == 2*num_interv_each,
           shift_noise_sd == 7,
           sd_hiddens == 5) %>%
    ggplot(aes(x = num_interv_each,
               y = AUC_mean,
               col = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type)


############# Old code (to use for updating the above, then remove)

### Permuted experiments

### Small (need to change)

AUC_alltargets <- tibble(readRDS("data/small/AUC_alltargets.rds")) 
AUC_alltargets_wide <- wide_AUC(AUC_alltargets)

AUC_singletargets <- tibble(readRDS("data/small/AUC_singletargets.rds"))
AUC_singletargets_wide <- wide_AUC(AUC_singletargets)

### stor1

AUC_alltargets_stor1 <- tibble(readRDS('data/stor1/AUC_alltargets.rds'))
AUC_alltargets_stor1_wide <- wide_AUC(AUC_alltargets_stor1)

AUC_singletargets_stor1 <- tibble(readRDS('data/stor1/AUC_singletargets.rds'))
AUC_singletargets_stor1_wide <- wide_AUC(AUC_singletargets_stor1)

### Singletargets

# singletargets_2_X_Y_2X_sdw7_sdh5 where Y in {15, 30}

AUC_singletargets_stor1_wide %>% 
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

AUC_alltargets_stor1_wide %>% 
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
# obs per environment: 2 resp. 10.
Shift noise sd: 7.
Hiddens sd: 5"
)

## alltargets_y_x_sdw7_sdh5
AUC_alltargets_stor1_wide %>% 
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

