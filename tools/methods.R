library(InvariantCausalPrediction)
library(MASS)
library(Matrix)
library(igraph)

## Parameter estimates

# beta estimate from unpermuted OLS
beta_OLS <- function(dat) {
  solve(crossprod(dat$X), crossprod(dat$X, dat$Y))
}

# Beta estimate from OLS with with permuted Y but unpermuted X
beta_POLS <- function(dat) solve(crossprod(dat$X), crossprod(dat$X, dat$Y_tilde))

# Beta estimate with permuted Y and using both permuted and unpermuted X
# (beta_hat_CR from Niklas' notes)
beta_DPOLS <- function(dat) solve(crossprod(dat$X, dat$X_tilde),
                                     crossprod(dat$X, dat$Y_tilde))

## p-values

p_values_ICP <- function(dat) {
  ICP(dat$X, as.vector(dat$Y), dat$setting,
      showAcceptedSets = F,
      showCompletion = F)$pvalues
}

# p-value based on OLS
p_values_OLS <- function(dat) {
  summary(lm(dat$Y ~ as.matrix(dat$X) - 1))$coefficient[,4]
}

# p-value based on POLS
p_values_POLS <- function(dat) {
  summary(lm(dat$Y_tilde ~ as.matrix(dat$X) - 1))$coefficient[,4]
}

# p-value based on DPOLS
p_values_DPOLS <- function(dat) {
  sigma <- summary(lm(dat$Y_tilde ~ as.matrix(dat$X_tilde)))$sigma
  cp <- solve(crossprod(dat$X, dat$X_tilde))
  std_errors <- sigma * sqrt(diag(cp %*% crossprod(dat$X) %*% t(cp)))
  t <- as.vector(beta_DPOLS(dat) / std_errors)
  pt(t, df = nrow(dat$X) - ncol(dat$X), lower.tail = F)
}


# Checking significance:
# returns the vertex numbers of the X's corresponding to beta entries
# that are significantly different from zero
# reg: Based on OLS
nonzero_OLS <- function(DAG_data, alpha = 0.05) {
  DAG_data$x[p_values_reg(DAG_data$dat) <= alpha]

}
# Based on POLS
nonzero_POLS <- function(DAG_data, alpha = 0.05) {
  DAG_data$x[p_values_beta_hat1(DAG_data$dat) <= alpha]
}

# Based on DPOLS
nonzero_DPOLS <- function(DAG_data, alpha = 0.05) {
  DAG_data$x[p_values_beta_hat2(DAG_data$dat) <= alpha]
}

# Says that all x are parents/ancestors
nonzero_all <- function(DAG_data, alpha = 0.05) {
  DAG_data$x
}

# Says that no x are parents/ancestors
nonzero_none <- function(DAG_data, alpha = 0.05) {
  NULL
}

# ICP
nonzero_ICP <- function(DAG_data, alpha = 0.05){
  DAG_data$x[p_values_ICP(DAG_data$dat) <= alpha] 
}

# General nonzero function, calling the relevant nonzero function for method.
nonzero <- function(method, DAG_data, alpha = 0.05) {
  do.call(str_c("nonzero_", method), list(DAG_data = DAG_data, alpha = alpha))
}

# List of nonzeros for each method in methods
nonzero_list <- function(DAG_data, methods, alpha = 0.05) {
  nonzeros <- lapply(methods, nonzero, DAG_data, alpha)
  names(nonzeros) <- methods
  return(nonzeros)
}

