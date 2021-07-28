# Synthetic example 3
#devtools::install()
#devtools::reload("/Users/pleckod/Library/R/3.6/library/fairadapt")
library(fairadapt)
library(reshape2)
library(ggplot2)

adjacency.matrix <- array(0, dim = c(6, 6))
colnames(adjacency.matrix) <- c("A", "Y", "X1", "X2", "X3", "X4")
rownames(adjacency.matrix) <- colnames(adjacency.matrix)
adjacency.matrix["A", c("X1", "X2")] <- 1
adjacency.matrix[c("X1", "X2"), "X3"] <- 1
adjacency.matrix[c("X2", "X3"), "X4"] <- 1
adjacency.matrix[c("X1", "X2", "X3", "X4"), "Y"] <- 1
OracleGen <- function(n) {
  expit <- function(x) return(exp(x)/(1+exp(x)))
  A <- rbinom(n, size = 1, prob = 0.5)
  Atilde <- A * 0
  n1 <- runif(n)
  n2 <- runif(n)
  n3 <- rnorm(n)
  n4 <- rnorm(n)
  recsub <- function(A, n1, n2, n3, n4) {
    X1 <-  n1 + (A == 0)*1/2
    X2 <-  n2 + (A == 0)*1/2
    X3 <-  X1*X2 + X1^2 + n3
    X4 <- 1/4*X3^2 + 2*X2^2 + n4
    Y <- rbinom(n, size = 1, prob = expit(1/2*(X1+X2+X3+X4-7)))
    df <- data.frame(cbind(Y,A,X1,X2,X3,X4))
    colnames(df) <- c("Y","A","X1","X2","X3","X4")
    return(df)
  }

  return(list(
    recsub(A, n1, n2, n3, n4),
    recsub(Atilde, n1, n2, n3, n4)
  ))
}

FrameSimilarity <- function(df1, df2) {
  if(!all(names(df1) == names(df2))) stop("data.frames not the same")
  return(sapply(names(df1), function(x) mean((as.numeric(df1[, x]) - as.numeric(df2[, x]))^2)))
}

n <- 1000
data <- OracleGen(n)
test.data <- OracleGen(10)[[1]]
train.data <- data[[1]]
oracle.train <- data[[2]]

GoT <- function(train.data, test.data, oracle.train, quant.method) {
  ind <- train.data$A == 1
  L <- fairadapt::fairadapt(Y ~ ., train.data = train.data,
    test.data = test.data, protect.A = "A",
    adj.mat = adjacency.matrix, quant.method = quant.method)
  adapted.train.data <- L[[1]]
  return(FrameSimilarity(adapted.train.data[ind, ], oracle.train[ind, ]))
}

methods <- "nn" #c("forest", "linear", "forest2")

L <- lapply(methods, function(x) GoT(train.data, test.data, oracle.train, quant.method = x))

df <- data.frame(Reduce(rbind, L))
df <- cbind(df, method = methods)
df <- melt(df, id.vars = "method")

ggplot(df, aes(x = factor(variable), y = value, color = method)) +
  geom_point(size = 3) + theme_bw(15) + ylab("Adaptation MSE") + xlab("Variable") +
  ggtitle("Goodness of Adaptation Method")
