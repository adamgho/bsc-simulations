## ROC curves

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
    )
)

get_ROC_curves <- function(tpr_fpr_file, id) {
    tibble(readRDS(tpr_fpr_file)) %>%
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

tpr_fpr_file <- 'localdata_old/alltargets_10_500_sdw7_sdh5/tpr_fpr.rds'
id <- 2

tibble(readRDS(tpr_fpr_file)) %>%
    filter(DAG_id == id, method == 'DPOLS-coef') ->
    tpr_fpr_tib

ggplot(tpr_fpr_tib, aes(x = fpr_pa, y = tpr_pa)) +
    geom_abline(lty = 2) +
    geom_polygon(data = rbind(tpr_fpr_tib[c('fpr_pa', 'tpr_pa')],
                              c(1,1),
                              c(1,0),
                              c(0,0)),
                 alpha = 0.15) +
    geom_point(col = '#ff8059') +
    geom_line(col = '#ff8059') +
    labs(x = 'False Positive Rate',
         y = 'True Positive Rate',
         title = 'ROC curve - parents') +
    annotate('text', x = 0.7, y = 0.25,
             label = 'AUC',
             size = 5)
    

ggsave('~/presentation/figures/roc_curve.pdf',
       width = 4,
       height = 4)

ggplot(tpr_fpr_tib, aes(x = fpr_anc, y = tpr_anc)) +
    geom_abline(lty = 2) +
    geom_polygon(data = rbind(tpr_fpr_tib[c('fpr_anc', 'tpr_anc')],
                              c(1,1),
                              c(1,0),
                              c(0,0)),
                 alpha = 0.15) +
    geom_point(col = '#ff8059') +
    geom_line(col = '#ff8059') +
    labs(x = 'False Positive Rate',
         y = 'True Positive Rate',
         title = 'ROC curve - ancestors') +
    annotate('text', x = 0.7, y = 0.25,
             label = 'AUC',
             size = 5)
    

ggsave('~/presentation/figures/roc_curve_anc.pdf',
       width = 4,
       height = 4)

