## ROC curves

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

## Old code (doesn't work with new version. Add full file names.)
rc <- get_ROC_curves('alltargets_10_500_sdw7_sdh5', 3)
rc[1]
rc[2]

# ROC curve to compare with ICP for alltargets_200_500
icp <- get_ROC_curves('alltargets_200_500_sdw7_sdh5')
icp[1]
icp[2]
