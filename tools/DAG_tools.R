suppressWarnings(suppressMessages(library(igraph)))
suppressWarnings(suppressMessages(library(Matrix)))

## Random DAGs are saved as S3 objects of the class "DAG_data".
## A DAG_data object contains the indices x, y, and, and h of X, Y, and H,
## the igraph opbject iG of the entailed DAG and vectors of the ancestors
## of y (anc_y) and the parents of y (pa_y). It also contains a random matrix
## B giving coefficients that can be used to get a linear SCM later.
## A DAG_data object can then be passed to one of the sim-functions that
## simulate data from a linear SCM where the structural assignments are given
## by the coefficients in B.
## The datasets are added to a list called dat_list. Most functions included
## here for working with DAG_data requires that there is only one data set
## in the dat_list.

# Gives random DAG. Uses causalOrder returned by randomDAG, selects h from
# the beginning (so they are upstream from x and y) and selects y in the
# middle of the rest of the causal order.
# So there will always be
# n_h + n_x/2 nodes before y in the causal order, that each
# have a probability of prob_connect of becoming a parent of y.
get_random_DAG <- function(n_x = 30,
                              n_h = 30,
                           prob_connect = 0.4) {
  num_var <- n_x + n_h + 1
  rDAG <- randomDAG(num_var, prob_connect)
  G <- rDAG$DAG
  iG <- graph_from_adjacency_matrix(G) # G as igraph object
  B <- randomB(G)
  h <- rDAG$causalOrder[1:n_h] # H must be upstream from X
  # y is chosen in the middle of the causal order used by randomDAG
  y <- rDAG$causalOrder[(n_h+1):num_var][round(num_var - n_h) / 2]
  # x is now completely determined as the remaining indices
  x <- setdiff(1:num_var, c(h, y))
  anc_y <- anc(iG, y)
  pa_y <- pa(iG, y)
  # Returns relevant data packaged as a DAG_data S3 object.
  structure(
    mget(c("B", "x", "y", "h", "iG", "anc_y", "pa_y")),
    class = "DAG_data"
  )
}

# plot method for DAG_data S3 class.
# The orange are X, the yellow are H, and the blue is Y.
# Hard to see structure when there are many nodes.
plot.DAG_data <- function(DAG_data) {
  # Vector indicating colors of the nodes
  col <- rep(NA, length(DAG_data$x) + length(DAG_data$h) + 1)
  # x is orange
  col[DAG_data$x] <- 1
  # y is blue
  col[DAG_data$y] <- 2
  # h is yellow
  col[DAG_data$h] <- 4
  plot(DAG_data$iG, vertex.color = col, edge.arrow.size = 0.5)
}

# print method for DAG_data S3 class.
# prints the indices of the X's, Y, the hiddens, the parents of Y,
# and the ancestors of Y.
print.DAG_data <- function(DAG_data) {
  print(DAG_data[c("x", "y", "h", "pa_y", "anc_y")])
}

# Gives ancestors of y in the igraph iG
anc <- function(iG, y) setdiff(subcomponent(iG, y, "in"), y)

# Gives parents of y in the igraph iG
pa <- function(iG, y) neighbors(iG, y, "in")

randomB <- function(G,lB = 0.1,uB = 0.9,twoIntervals = 1)
  # if twoIntervals == TRUE, lB and uB should be positive
  # Copyright (c) 2012-2012  Jonas Peters [peters@stat.math.ethz.ch]
  # All rights reserved.  See the file LICENSE.md for license terms.
{
  numCoeff <- sum(G)
  B <- t(G)
  if(numCoeff ==1)
  {
    coeffs <- sample(c(-1,1),size=numCoeff,0.5)^(twoIntervals) * runif(1,lB,uB)
  }
  else
  {
    coeffs <- diag(sample(c(-1,1),size=numCoeff,0.5)^(twoIntervals)) %*% runif(numCoeff,lB,uB)
  }
  B[B==1] <- coeffs
  return(B)
}

computeCausOrder <- function(G)
  # Copyright (c) 2013  Jonas Peters  [peters@stat.math.ethz.ch]
  # All rights reserved.  See the file LICENSE.md for license terms. 
{
  p <- dim(G)[2]
  remaining <- 1:p
  causOrder <- rep(NA,p)
  for(i in 1:(p-1))
  {
    root <- min(which(colSums(G) == 0))
    causOrder[i] <- remaining[root]
    remaining <- remaining[-root]
    G <- G[-root,-root]
  }
  causOrder[p] <- remaining[1]    
  return(causOrder)
}

randomDAG <- function(p,probConnect,causalOrder = sample(p,p,replace=FALSE))
  # Adapted slightly to also return causalOrder.
  # Original unadapted version written by Jonas Peters:
  # Copyright (c) 2010 - 2012  Jonas Peters  [peters@stat.math.ethz.ch]
  #    
  # All rights reserved.  See the file LICENSE.md for license terms. 
  # 
  #
  # simulates a directed acyclic graph (DAG) and returns its adjacency matrix
  # 
  # INPUT:
  #   p           number of nodes 
  #   probConnect the probability that an edge i -> j is added to the DAG
  #   causalOrder starting with sink node (also called topological order)
  #   
  # OUTPUT:
  # List containing:
  #   DAG           Adjacency matrix of a directed acyclic graph (DAG)
  #   causalOrder   The causalOrder used.
{
  #DAG <- as(diag(rep(0,p)),"sparseMatrix")
  DAG <- Matrix(nrow=p,ncol=p,0,sparse=TRUE)
  for(i in 1:(p-2))
  {
    node <- causalOrder[i]
    possibleParents <- causalOrder[(i+1):p]
    numberParents <- rbinom(n=1,size=(p-i),prob=probConnect)
    Parents <- sample(x = possibleParents, size = numberParents, replace = FALSE)
    DAG[Parents,node] <- rep(1,numberParents)
  }
  # Sample does not work properly when choosing from sets with one element. We thus consider the last case separately.  
  node <- causalOrder[p-1]
  ParentYesNo <- rbinom(n=1,size=1,prob=probConnect)
  DAG[causalOrder[p],node] <- ParentYesNo
  
  causalOrder <- rev(causalOrder)
  # Returns the DAG and the causal order.
  return(mget(c("DAG", "causalOrder")))
}

