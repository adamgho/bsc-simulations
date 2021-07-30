## Loops through the all_AUC.rds files in all subdirectories of your current
## directory.

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    args = '.'
}

suppressWarnings(suppressMessages(library(dplyr)))

res_tib <- tibble()
## Get filenames as character vector
all_AUC_name_list <- list.files('.', pattern = 'all_AUC.rds', recursive = TRUE)
## Loop through filenames, read file, and save AUC values.
for (filename in all_AUC_name_list) {
    res_tib <- rbind(
        res_tib,
        filter(readRDS(filename),
               method == 'randomguess')[c('AUC_anc', 'AUC_pa')]
    )
}

## Gives first quartile
quartile1 <- function(x) quantile(x, 0.25)
## Gives third quartile
quartile3 <- function(x) quantile(x, 0.75)

## Saves summary values (mean, median, quartile1, and quartile3)
res_tib %>%
    summarise(across(.fns = c(mean, median, quartile1, quartile3))) %>%
    (function(dat) {
        colnames(dat) <- c(
            paste(
                'AUC_anc',
                c('mean', 'median', 'quartile1', 'quartile3'),
                sep = '_'
            ),
            paste(
                'AUC_pa',
                c('mean', 'median', 'quartile1', 'quartile3'),
                sep = '_'
            )
        )
        return(dat)
    }) %>%
    write.table('random_AUC.txt', row.names = FALSE)

## Saves all random AUC
write.table(res_tib, 'collected_AUC_randomguess.txt', row.names = FALSE)
