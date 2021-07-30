## Uncomment the following line if you use ICP
suppressWarnings(suppressMessages(library(InvariantCausalPrediction)))
suppressWarnings(suppressMessages(library(MASS)))
suppressWarnings(suppressMessages(library(Matrix)))
# suppressWarnings(suppressMessages(library(igraph)))

### Parameter estimates
### Input: dat from a DAG_data object (pass DAG_data$dat)

# beta_OLS_hat
beta_OLS <- function(dat) solve(crossprod(dat$X), crossprod(dat$X, dat$Y))

# beta_POLS_hat
beta_POLS <- function(dat) solve(crossprod(dat$X), crossprod(dat$X, dat$Y_tilde))

# beta_DPOLS_hat
beta_DPOLS <- function(dat) solve(crossprod(dat$X, dat$X_tilde),
                                     crossprod(dat$X, dat$Y_tilde))

### p-values
### Input: dat from a DAG_data object (pass DAG_data$dat)

# p-values from ICP
p_values_ICP <- function(dat) {
  ICP(dat$X, as.vector(dat$Y), dat$setting,
      showAcceptedSets = F,
      showCompletion = F)$pvalues
}

## p-values from PICP (Permuted ICP)
p_values_PICP <- function(dat) {
  ICP(dat$X, as.vector(dat$Y_tilde), dat$setting,
      showAcceptedSets = F,
      showCompletion = F)$pvalues
}

# p-value from OLS
p_values_OLS <- function(dat) {
  summary(lm(dat$Y ~ as.matrix(dat$X) - 1))$coefficient[,4]
}

# p-value from POLS
p_values_POLS <- function(dat) {
  summary(lm(dat$Y_tilde ~ as.matrix(dat$X) - 1))$coefficient[,4]
}

# p-value from DPOLS
p_values_DPOLS <- function(dat) {
  sigma <- summary(lm(dat$Y_tilde ~ as.matrix(dat$X_tilde)))$sigma
  cp <- solve(crossprod(dat$X, dat$X_tilde))
  std_errors <- sigma * sqrt(diag(cp %*% crossprod(dat$X) %*% t(cp)))
  t <- as.vector(beta_DPOLS(dat) / std_errors)
  ## df is the number of X's minus the number of observations
  2 * pt(abs(t), df = nrow(dat$X) - ncol(dat$X), lower.tail = F)
}

## mean-shift baseline method
get_meanshifts <- function(dat) {
    ## Indices of control observations
    control_indices <- dat$interv == 0
    ## List of X's intervened on
    intervs <- unique(dat$interv)
    ## Indices of X's
    x <- as.numeric(substr(colnames(dat$X), 2, 3))
    ## Vector for storing results
    nx <- length(x)
    meanabsmean <- rep(0, nx)    
    ## Loop through all X
    for (i in 1:nx) {
        ## If x is never intervened on, just let absmean be 0 (since we have no info)
        ## If x is intervened on calculate absmean as follows
        if (x[i] %in% intervs) {
            ## Settings where x is intervened on
            settings <- unique(dat$setting[dat$interv == x[i]])
            absmean_sum <- 0
            ## Calculate p-value in each setting with intervention on x
            for (setting in settings) {
                setting_indices <- dat$setting == setting
                absmean_sum <- absmean_sum + abs(mean(dat$Y[setting_indices]))
            }
            ## Save mean of absmeans
            meanabsmean[i] <- mean(absmean_sum)
        }
    }
    return(meanabsmean)
}

### Checking significance
### returns the vertex numbers of the X's corresponding to beta entries
### that are significantly different from zero
### Inputs:
###   DAG_data: A DAG_data object with a dat attribute.
###   alpha: significance level.

nonzero_OLS <- function(DAG_data, alpha = 0.05) {
  DAG_data$x[p_values_reg(DAG_data$dat) <= alpha]

}

nonzero_POLS <- function(DAG_data, alpha = 0.05) {
  DAG_data$x[p_values_beta_hat1(DAG_data$dat) <= alpha]
}


nonzero_DPOLS <- function(DAG_data, alpha = 0.05) {
  DAG_data$x[p_values_beta_hat2(DAG_data$dat) <= alpha]
}

# ICP
nonzero_ICP <- function(DAG_data, alpha = 0.05){
  DAG_data$x[p_values_ICP(DAG_data$dat) <= alpha] 
}

# Says that all x are parents/ancestors
nonzero_all <- function(DAG_data, alpha = 0.05) {
  DAG_data$x
}

# Says that no x are parents/ancestors
nonzero_none <- function(DAG_data, alpha = 0.05) {
  NULL
}

# General nonzero function, calling the relevant nonzero function for method.
# E.g if method is "POLS" then it calls nonzero_POLS
nonzero <- function(method, DAG_data, alpha = 0.05) {
  do.call(str_c("nonzero_", method), list(DAG_data = DAG_data, alpha = alpha))
}

# List of nonzeros for each method in methods.
# Input:
#   DAG_data: A DAG_data object with dat attribute
#   methods: vector of strings each designating a method.
#   alpha: significance level
# Ouput:
#   List nonzeroes where nonzeros[[method]] is the outcome of nonzero called
#   for that method.
nonzero_list <- function(DAG_data, methods, alpha = 0.05) {
  nonzeros <- lapply(methods, nonzero, DAG_data, alpha)
  names(nonzeros) <- methods
  return(nonzeros)
}

