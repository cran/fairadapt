# get the confounded components from a graph
### functionality
CComponent <- function(variable, c.mat) {
  if(!identical(c.mat, t(c.mat))) stop("Confounding matrix not symmetric")
  c.mat <- c.mat + diag(rep(1, nrow(c.mat)))
  c.comp <- c.mat
  for(i in 1:nrow(c.mat)) {
    c.comp <- c.comp %*% c.mat + c.mat
  }
  c.ind <- c.comp[, variable] > 0
  return(rownames(c.mat)[c.ind])
}

GetParents <- function(variable, adjacency.matrix) {
  parent.indicators <- adjacency.matrix[, variable] == 1
  parents <- row.names(adjacency.matrix)[parent.indicators]
  return(parents)
}

GetAncestors <- function(variable, adjacency.matrix) {
  matrix.size <- dim(adjacency.matrix)
  num.walks <- adjacency.matrix
  for (i in 1:matrix.size[1]) {
    num.walks <- adjacency.matrix + num.walks %*% adjacency.matrix
  }
  ancestor.indicators <- num.walks[, variable] > 0
  ancestors <- colnames(adjacency.matrix)[ancestor.indicators]
  return(ancestors)
}

SMParents <- function(curr.var, adj.mat, c.mat) {
  sm.par <- lapply(CComponent(curr.var, c.mat), function(v) GetParents(v, adj.mat))
  sm.par <- union(Reduce(union, sm.par), CComponent(curr.var, c.mat))
  sm.par <- intersect(sm.par, GetAncestors(curr.var, adj.mat))
  return(sm.par)
}

# Example 1 
{
  c.mat <- array(0, dim = c(5, 5))
  rownames(c.mat) <- colnames(c.mat) <- paste0("X", 1:5)
  c.mat["X1", "X3"] <- c.mat["X3", "X1"] <- 1
  c.mat["X3", "X5"] <- c.mat["X5", "X3"] <- 1
  c.mat["X2", "X4"] <- c.mat["X4", "X2"] <- 1
  
  adj.mat <- array(0, dim = c(5, 5))
  rownames(adj.mat) <- colnames(adj.mat) <- paste0("X", 1:5)
  for(i in 1:4) adj.mat[i, i+1] <- 1
  
  lapply(paste0("X", 1:5), function(x) SMParents(x, adj.mat, c.mat))
}

# Example 2
{
  c.mat <- array(0, dim = c(4, 4))
  rownames(c.mat) <- colnames(c.mat) <- paste0("X", 1:4)
  c.mat["X2", "X3"] <- c.mat["X3", "X2"] <- 1
  
  adj.mat <- array(0, dim = c(4, 4))
  rownames(adj.mat) <- colnames(adj.mat) <- paste0("X", 1:4)
  for(i in 1:3) adj.mat[i, i+1] <- 1
  
  lapply(paste0("X", 1:4), function(x) SMParents(x, adj.mat, c.mat))
}

# Example 3
{
  c.mat <- array(0, dim = c(4, 4))
  rownames(c.mat) <- colnames(c.mat) <- paste0("X", 1:4)
  c.mat["X2", "X3"] <- c.mat["X3", "X2"] <- 1
  c.mat["X1", "X4"] <- c.mat["X4", "X1"] <- 1
  
  adj.mat <- array(0, dim = c(4, 4))
  rownames(adj.mat) <- colnames(adj.mat) <- paste0("X", 1:4)
  for(i in 1:3) adj.mat[i, i+1] <- 1
  
  lapply(paste0("X", 1:4), function(x) SMParents(x, adj.mat, c.mat))
}





