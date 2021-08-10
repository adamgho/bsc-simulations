## Used to generate plots for thesis

## Note that this will not run for you, since it relies on the results being
## saved in the exact locations that I used, and in the six different portions
## that I split them into for practical reasons.

library(tidyverse)
theme_set(
    theme_bw(base_size=12, base_family="") %+replace%
    theme(
        panel.grid = element_blank(),
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
        plot.title = element_text(size = 12,
                                  hjust = 0),
        plot.subtitle = element_text(size = 10,
                                     margin = margin(t = 5, b = 3),
                                     hjust = 0)
    )
)

## Plots will be converted to tikz code saved in tex files.
library(tikzDevice)

## Width and height of results images (in inches)
imw <- 6.5
imh <- 8.5

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

####### 30 X and 30 H

## Reads randomguess performance
read.table('local_results/all_random_AUC_summary.txt',
           header = TRUE) %>%
    pivot_longer(cols = everything(),
                 names_to = c('type', 'measure'),
                 names_prefix = 'AUC_',
                 names_sep = '_') %>%
    pivot_wider(names_from = measure,
                names_prefix = 'AUC_',
                values_from = value) %>%
    mutate(type = nicer_type(type)) ->
    random30

## random performance with guessing after parents
read.table('local_results/pa_then_random_AUC_summary.txt',
           header = TRUE) %>%
    pivot_longer(cols = everything(),
                 names_to = c('type', 'measure'),
                 names_prefix = 'AUC_',
                 names_sep = '_') %>%
    pivot_wider(names_from = measure,
                names_prefix = 'AUC_',
                values_from = value) %>%
    mutate(type = nicer_type(type)) %>%
    filter(type == 'Ancestors') ->
    random_after_pa

## change colnames so they don't match those of AUC_all and AUC_single from
## below
colnames(random30) <- paste('r', colnames(random30), sep = '_')
colnames(random30)[1] <- 'type'

colnames(random_after_pa) <- paste('rp', colnames(random_after_pa), sep = '_')
colnames(random_after_pa)[1] <- 'type'

random_collected <- left_join(random30, random_after_pa, by = 'type')

## Lines and ribbons to add to all ggplots
random_lines <- list(
    geom_hline(aes(yintercept = r_AUC_mean,
                   lty = 'all-random')),
    geom_hline(aes(yintercept = rp_AUC_mean,
                   lty = 'random-after-parents')),
    geom_ribbon(aes(ymin = rp_AUC_quartile1,
                    ymax = rp_AUC_quartile3,
                    ## Hack to get it to only use x-values once. Otherwise the
                    ## x-values for each method would add another layer of fill,
                    ## meaning that the alpha would effectively depend on the
                    ## number of methods, and thus differ between the different
                    ## plots (since some plots also conain, e.g., ICP and PICP).
                    ## The solution is to only add fill for the x-values
                    ## corresponding to OLS, since this method is included
                    ## in all plots.
                    alpha = ifelse(method == 'OLS',
                                   'a',
                                   'b')),
                lty = 0),
    geom_ribbon(aes(ymin = r_AUC_quartile1,
                    ymax = r_AUC_quartile3,
                    ## See explanation above
                    alpha = ifelse(method == 'OLS',
                                   'a',
                                   'b')),
                lty = 0),
    scale_linetype_manual(name = '$\\mathrm{random\\ baselines}\\quad$',
                          values = c('dashed', 'dotted')),
    scale_alpha_manual(name = '$\\mathrm{random\\ baselines}\\quad$',
                       values = c(0.07, 0),
                       guide = 'none'),
    guides(colour = guide_legend(override.aes = list(fill = 0.01)))
)

## alltargets AUC (permuted - Problem B)
stor1_all <- read_wide('local_results/stor1/AUC_alltargets.rds')
stor2_all <- read_wide('local_results/stor2/AUC_alltargets.rds')
stor4_all <- read_wide('local_results/stor4/AUC_alltargets.rds')
## alltargets AUC (separate - Problem A)
stor3_all <- read_wide('local_results/stor3/AUC_alltargets.rds')
stor5_all <- read_wide('local_results/stor5/AUC_alltargets.rds')
stor6_all <- read_wide('local_results/stor6/AUC_alltargets.rds')

## Collect and add problem column describing whether the data is from separate
## experiments (Problem A) or permuted (Problem B).
rbind(stor1_all, stor2_all, stor4_all) %>%
    mutate(problem = 'Permuted') %>%
    rbind(rbind(stor3_all, stor5_all, stor6_all) %>%
          mutate(problem = 'Truly separate')) %>%
    filter(method != 'randomguess') %>%
    mutate(type = nicer_type(type),
           method = fct_recode(method,
                               OLS = 'OLS-coef',
                               POLS = 'POLS-coef',
                               DPOLS = 'DPOLS-coef')) ->
    AUC_all

## singletargets AUC
## permuted (Problem B)
stor1_single <- read_wide('local_results/stor1/AUC_singletargets.rds')
stor2_single <- read_wide('local_results/stor2/AUC_singletargets.rds')
## separate (Problem A)
stor5_single <- read_wide('local_results/stor5/AUC_singletargets.rds')
stor6_single <- read_wide('local_results/stor6/AUC_singletargets.rds')

rbind(stor1_single, stor2_single) %>%
    mutate(problem = 'Permuted') %>%
    rbind(mutate(rbind(stor5_single, stor6_single),
                 problem = 'Truly separate')) %>%
    filter(method != 'randomguess') %>%
    mutate(type = nicer_type(type),
           method = fct_recode(method,
                               OLS = 'OLS-coef',
                               POLS = 'POLS-coef',
                               DPOLS = 'DPOLS-coef')) ->
    AUC_single

## Color scales: Have to set colors manually - otherwise the methods get
## different colors in different plots (since there is a varying number of
## methods in the plots).
## Some of the colors (specifically '#44bc44', '#2fafff', '#ff8059', '#feacd0',
## and '#b6a0ff') are from the Modus Themes by Protesilaos Stavrou (GNU GPLv3
## licensed, see https://protesilaos.com/modus-themes/).

fct_recode(AUC_all$method,
           OLS = 'OLS',
           POLS = 'POLS',
           DPOLS = 'DPOLS')

main_methods <- c('OLS', 'OLS-pvals',
                  'POLS', 'POLS-pvals',
                  'DPOLS', 'DPOLS-pvals')


col_main_methods <- c('#44bc44', '#a9d566',
                      '#2fafff', '#97d7ff',
                      '#ff8059', '#ffbfac')

scale_main <- scale_color_manual(
    breaks = main_methods,
    values = col_main_methods
)

mean_shift <- 'mean-shift'
col_mean_shift <- 'yellow2'

scale_mean <- scale_color_manual(
    breaks = c(main_methods, mean_shift),
    values = c(col_main_methods, col_mean_shift))

ICPs <- c('ICP', 'PICP')
col_ICPs <- c('#feacd0', '#b6a0ff')

scale_ICPs <- scale_color_manual(
    breaks = c(main_methods, ICPs, 'all-random'),
    values = c(col_main_methods, col_ICPs, '#000000'))

scale_all <- scale_color_manual(
    breaks = c(main_methods, ICPs, mean_shift, 'all-random'),
    values = c(col_main_methods, col_ICPs, col_mean_shift, '#000000'))

##### 30 X and 30 H

## Used to label n_obs_each in facet_grid
label_obs <- function(n_obs_each) {
    sprintf('$\\mathtt{no} = %s$', n_obs_each)
}

## Collect AUC for methods and random baselines
AUC_all <- left_join(AUC_all, random_collected, by = 'type') %>%
    mutate(is_OLS_coef = method == 'OLS')
AUC_single <- left_join(AUC_single, random_collected, by = 'type')

tikz(file = '~/thesis/figures/alltargets_vary_num_interv.tex', width=imw, height=imh)

AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_hline(aes(yintercept = 1), alpha = 0.1) +
    random_lines +
    geom_line() +
    geom_point(size = 0.8) +
    facet_grid(n_obs_each + type ~ problem,
               labeller = labeller(n_obs_each = label_obs)) +
    scale_main +
    labs(x = "\\texttt{ne} (number of environments)", y = "Average AUC",
         title = "Varying number of environments.",
         subtitle = "alltargets; $\\mathtt{sdw} = 7, \\mathtt{sdh} = 5$; 30 $X$'s and 30 $H$'s.",) +
    coord_cartesian(ylim = c(0.5, NA))

endoffile <- dev.off()


## The following plot contains ribbons giving the first and third quartiles of
## the AUC for each method

tikz(file = '~/thesis/figures/alltargets_vary_num_interv_ribbons.tex', width=imw, height=imh)

AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each %in% c(2, 10),
           method %in% c('OLS',
                         'POLS',
                         'DPOLS')) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_hline(aes(yintercept = 1), alpha = 0.1) +
    random_lines + 
    geom_line() +
    geom_line(aes(y = AUC_median), lty = 'dotted') +
    geom_ribbon(aes(ymin = AUC_quartile1,
                    ymax = AUC_quartile3,
                    fill = method),
                alpha = 0.05,
                lty = 2) +
    geom_point(size = 0.8) +
    facet_grid(n_obs_each + type ~ problem,
               labeller = labeller(n_obs_each = label_obs)) +
    scale_main +
    labs(x = "\\texttt{ne} (number of environments)", y = "AUC",
         title = "Comparing quartiles in Figure \\ref{fig:vary-ne} setting.",
         subtitle = "Median (dotted), 1st and 3rd quartile (dashed), and average (solid).") +
    scale_fill_manual(guide = 'none',
                      breaks = main_methods,
                      values = col_main_methods) +
    coord_cartesian(ylim = c(0.5, NA))

endoffile <- dev.off()


tikz(file = '~/thesis/figures/alltargets_x_500_7_5.tex', width=imw, height=imh)

AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           num_interv == 500) %>% 
    ggplot(aes(x = n_obs_each, y = AUC_median, col = method)) +
    geom_hline(aes(yintercept = 1), alpha = 0.1) +
    random_lines +
    geom_line() +
    geom_point() +
    facet_grid(type ~ problem) +
    scale_main +
    labs(
        x = "\\texttt{no} (number of observations per environment)",
        y = "Average AUC",
        title =
            "Varying number of observations per environment.",
        subtitle =
            "alltargets; $\\mathtt{ne} = 500, \\mathtt{sdw} = 7, \\mathtt{sdh} = 5$; 30 $X$'s and 30 $H$'s."
    ) +
    coord_cartesian(ylim = c(0.5, NA))

endoffile <- dev.off()

tikz(file = '~/thesis/figures/alltargets_5000obs.tex', width=imw, height=imh)

    
## There are a couple of points here, where I ran two different simulations over
## 1000 DAGs, so I first take the average of these two averages, to not get
## multiple points with the same x-coordinate (which would give saw-tooth jumps
## in the plot)

AUC_all %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           num_interv*n_obs_each == 5000) %>%
    group_by(n_obs_each,
             num_interv,
             shift_noise_sd,
             sd_hiddens,
             method,
             type,
             problem) %>%
    summarise(AUC_mean = mean(AUC_mean)) %>%
    left_join(random_collected, by = 'type') %>% # random was dropped at summarise
    ggplot(aes(x = log(num_interv / n_obs_each), y = AUC_mean, col = method)) +
    random_lines +
    geom_line() +
    geom_point() +
    scale_main +
    facet_grid(type ~ problem) +
       labs(
           x = "$\\log \\frac{\\mathtt{ne}\\mathrm{\\ (number\\ of\\ environments)}}{\\mathtt{no}\\mathrm{\\ (number\\ of\\ observations\\ per\\ environment)}}$",
           y = "Average AUC",
           title =
               "Few env. with many obs. vs. many env. with few obs.",
           subtitle =
               "alltargets; $\\mathtt{no}\\cdot\\mathtt{ne} = 5000, \\mathtt{sdw} = 7, \\mathtt{sdh} = 5$; 30 $X$'s and 30 $H$'s."
       ) +
    coord_cartesian(ylim = c(0.5, NA))

endoffile <- dev.off()

label_obs_extra <- function(n_obs_each) {
    ifelse(n_obs_each == 2,
           '{\\scriptsize $(\\mathtt{no}, \\mathtt{ne}) = (2,2500)$}',
           '{\\scriptsize $(\\mathtt{no}, \\mathtt{ne}) = (10,500)$}')
}


tikz(file = '~/thesis/figures/alltargets_vary_sdw.tex', width=imw, height=imh)

AUC_all %>% 
    filter(sd_hiddens == 5,
           (num_interv == 500 & n_obs_each == 10) |
           (num_interv == 2500 & n_obs_each == 2)) %>% 
    ggplot(aes(x = shift_noise_sd, y = AUC_mean, col = method)) +
    geom_hline(aes(yintercept = 1), alpha = 0.1) +
    random_lines +
    geom_line() +
    geom_point() +
    scale_main +
    facet_grid(n_obs_each + type ~ problem,
               labeller = labeller(n_obs_each = label_obs_extra)) +
       labs(
           x = "\\texttt{sdw} (standard deviation of mean shifts)",
           y = "Average AUC",
           title =
               "Varying standard deviation of mean shifts $W$.",
           subtitle =
               "alltargets; $\\mathtt{sdh} = 5$; 30 $X$'s and 30 $H$'s."
       )

endoffile <- dev.off()

tikz(file = '~/thesis/figures/alltargets_vary_sdh.tex', width=imw, height=imh)

AUC_all %>% 
    filter(shift_noise_sd == 7,
           (num_interv == 500 & n_obs_each == 10) |
           (num_interv == 2500 & n_obs_each == 2)) %>% 
    ggplot(aes(x = sd_hiddens, y = AUC_mean, col = method)) +
    random_lines +
    geom_line() +
    geom_point() +
    scale_main +
    facet_grid(n_obs_each + type ~ problem,
               labeller = labeller(n_obs_each = label_obs_extra)) +
       labs(
           x = "\\texttt{sdh} (standard deviation of hidden variables)",
           y = "Average AUC",
           title =
               "Varying standard deviation of hidden variables.",
           subtitle =
               "alltargets; $\\mathtt{sdw} = 7$; 30 $X$'s and 30 $H$'s."
       )

endoffile <- dev.off()

## singletargets

label_num_x_interv <- function(num_x_interv) {
    sprintf("$\\mathtt{nxi} = %s$", num_x_interv)
}

label_obs_brief <- function(n_obs_each) {
    paste(n_obs_each, 'obs./env.')
}

## TODO: Fix scales on the short Problem A x-axis so there is only a 0 and 500.
tikz(file = '~/thesis/figures/singletargets_z_x_y_zx_7_5.tex', width=imw, height=imh)

AUC_single %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each %in% c(2, 10),
           num_x_interv %in% c(15, 30),
           n_obs_control == n_obs_each * num_interv_each) %>% 
    ggplot(aes(x = num_interv_each, y = AUC_mean, col = method)) +
    random_lines +
    geom_line() +
    geom_point() +
    scale_mean +
    facet_grid(num_x_interv + type ~ problem + n_obs_each,
               labeller = labeller(num_x_interv = label_num_x_interv,
                                   n_obs_each = label_obs)) +
       labs(
           x = "\\texttt{no} (number of observations per environment)",
           y = "Average AUC",
           title =
               "Varying number of observations per environment.",
           subtitle =
               "singletargets; $\\mathtt{noc} = \\mathtt{no}\\cdot \\mathtt{nei}, \\mathtt{sdw} = 7, \\mathtt{sdh} = 5$; 30 $X$'s and 30 $H$'s."
       )

endoffile <- dev.off()

##### 5 X and 5 H

## alltargets
read_wide("local_results/nx5/AUC_alltargets.rds") %>%
    mutate(problem = 'Permuted') %>%
    rbind(read_wide('local_results/nx5_sep/AUC_alltargets.rds') %>%
          mutate(problem = 'Truly separate')) %>%
    filter(method != 'randomguess') %>%
    mutate(type = nicer_type(type),
           method = fct_recode(method,
                               OLS = 'OLS-coef',
                               POLS = 'POLS-coef',
                               DPOLS = 'DPOLS-coef')) ->
    AUC_all_5

## singletargets
read_wide("local_results/nx5/AUC_singletargets.rds") %>%
    mutate(problem = 'Permuted') %>%
    rbind(read_wide('local_results/nx5_sep/AUC_singletargets.rds') %>%
          mutate(problem = 'Truly separate')) %>%
    filter(method != 'randomguess') %>%
        mutate(type = nicer_type(type),
               method = fct_recode(method,
                                   OLS = 'OLS-coef',
                                   POLS = 'POLS-coef',
                                   DPOLS = 'DPOLS-coef')) ->
    AUC_single_5
    
## random
read.table('local_results/nx5_all_random_AUC_summary.txt',
           header = TRUE) %>%
    pivot_longer(cols = everything(),
                 names_to = c('type', 'measure'),
                 names_prefix = 'AUC_',
                 names_sep = '_') %>%
    pivot_wider(names_from = measure,
                names_prefix = 'AUC_',
                values_from = value) %>%
    mutate(type = nicer_type(type)) ->
    random5

colnames(random5)[2:5] <- paste('r', colnames(random5)[2:5], sep = '_')

## join AUC for methods and random
AUC_all_5 <- left_join(AUC_all_5, random5, by = 'type')
AUC_single_5 <- left_join(AUC_single_5, random5, by = 'type')

## alltargets plot

tikz(file = '~/thesis/figures/alltargets_vary_num_interv_nx5.tex', width=imw, height=imh)

AUC_all_5 %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each %in% c(2, 10)) %>% 
    ggplot(aes(x = num_interv, y = AUC_mean, col = method)) +
    geom_hline(aes(yintercept = 1), alpha = 0.1) +
    geom_hline(aes(yintercept = r_AUC_mean, col = 'all-random'),
               lty = 2) +
    geom_line() +
    geom_point(size = 0.8) +
    facet_grid(n_obs_each + type ~ problem,
               labeller = labeller(n_obs_each = label_obs)) +
    scale_ICPs +
    labs(
        x = "\\texttt{ne} (number of environments)",
        y = "Average AUC",
        title =
            "Varying number of environments.",
        subtitle =
            "alltargets; $\\mathtt{sdw} = 7, \\mathtt{sdh} = 5$; 5 $X$'s and 5 $H$'s."
    ) +
    coord_cartesian(ylim = c(0.68, NA))

endoffile <- dev.off()

## singletargets plot

tikz(file = '~/thesis/figures/singletargets_z_x_y_zx_7_5_nx5.tex', width=imw, height=imh)

AUC_single_5 %>% 
    filter(shift_noise_sd == 7,
           sd_hiddens == 5,
           n_obs_each %in% c(2, 10),
           num_x_interv == 5,
           n_obs_control == n_obs_each * num_interv_each) %>% 
    ggplot(aes(x = num_interv_each, y = AUC_mean, col = method)) +
    geom_hline(aes(yintercept = 1), alpha = 0.1) +
    geom_hline(aes(yintercept = r_AUC_mean, col = 'all-random'),
               lty = 2) +    
    geom_line() +
    geom_point() +
    scale_all +
    facet_grid(problem + type ~ n_obs_each,
               labeller = labeller(num_x_interv = label_num_x_interv,
                                   n_obs_each = label_obs),
               scales = 'free_x',
               space = 'free_x') +
       labs(
           x = "\\texttt{no} (number of environments)",
           y = "Average AUC",
           title =
               "Varying number of observations per environment.",
           subtitle =
               "singletargets; $\\mathtt{nxi} = 5, \\mathtt{noc} = \\mathtt{no}\\cdot \\mathtt{nei}, \\mathtt{sdw} = 7, \\mathtt{sdh} = 5$; 5 $X$'s and 5 $H$'s."
       ) +
    coord_cartesian(ylim = c(0.68, NA))

endoffile <- dev.off()
