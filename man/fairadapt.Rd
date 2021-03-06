% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fair_Adaptation.R
\name{fairadapt}
\alias{fairadapt}
\title{fairadapt}
\usage{
fairadapt(formula, train.data, test.data, adj.mat, protect.A, res.vars = NULL)
}
\arguments{
\item{formula}{Object of class \code{formula} describing the response and the covariates.}

\item{train.data, test.data}{Training data & testing data, both of class \code{data.frame}.}

\item{adj.mat}{Matrix of class \code{matrix} encoding the relationships in the causal graph. \code{M[i,j] == 1} implies the existence of an edge from node i to node j. Must include all the}

\item{protect.A}{A value of class \code{character} describing the binary protected attribute. Must be one of the entries of \code{colnames(adj.mat)}.}

\item{res.vars}{A vector of class \code{character} listing all the resolving variables, which should not be changed by the adaption procedure. Default value is \code{NULL}, corresponding to no resolving variables. Resolving variables should be a subset of the descendants of the protected attribute.}
}
\value{
A \code{list} of length two. The two elements of the list are of class \code{data.frame} and contain the adapted training and testing data respectively.
}
\description{
Implementation of fair data adaptation with quantile preservation (Plecko & Meinshausen 2019).
Uses only plain \code{R}.
}
\details{
The procedure takes the training and testing data as an input, together with the causal graph given by an adjacency matrix and the list of resolving variables, which should be kept fixed during the adaptation procedure. The procedure then calculates a fair representation of the data, after which any classification method can be used. There are, however, several valid training options yielding fair predictions, and the best of them can be chosen with cross-validation. For more details we refer the user to the original paper.
Most of the running time is due to the quantile regression step using the ranger package.
}
\examples{

\dontshow{
adjacency.matrix <- array(0, dim = c(3,3))
colnames(adjacency.matrix) <- c("A","Y","X")
rownames(adjacency.matrix) <- colnames(adjacency.matrix)
adjacency.matrix["A", "X"] <- 1
adjacency.matrix["X", "Y"] <- 1
DataGen <- function(n) {
 expit <- function(x) return(exp(x)/(1+exp(x)))
 A <- rbinom(n, size = 1, prob = 0.5)
 coeff <- 1 / 4
 dev <- 1
 X <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
 Y <- rbinom(n, size = 1, prob = expit((X)))
 df <- data.frame(cbind(Y,A,X))
 colnames(df) <- c("Y","A","X")
 return(df)
}
fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
          adj.mat = adjacency.matrix, protect.A = "A")
}

library(fairadapt)
n1 <- n2 <- 100
n <- n1 + n2
A <- rbinom(n, size = 1, prob = 0.5)
X1 <- rnorm(n) + (A-1)
X2 <- rnorm(n) + (2*A-1)
Y <- rbinom(n, size = 1, prob = exp(X1+X2)/(1+exp(X1+X2)))
data <- data.frame(cbind(A, X1, X2, Y))
adjacency.matrix <- array(0, dim = c(4,4))
colnames(adjacency.matrix) <- rownames(adjacency.matrix) <- c("A", "X1", "X2", "Y")
adjacency.matrix["A", c("X1", "X2")] <- 1
adjacency.matrix[c("X1", "X2"), "Y"] <- 1
L <- fairadapt(Y ~ ., train.data = data[1:n1, ], test.data = data[-(1:n1), ],
               protect.A = "A", adj.mat = adjacency.matrix, res.vars = "X1")
\donttest{
library(fairadapt)

# UCI Adult example
L <- fairadapt(income ~ ., train.data = adult.train,
               test.data = adult.test, protect.A = "sex",
               adj.mat = adjacency.matrix)
adjusted.train.data <- L[[1]]
adjusted.test.data <- L[[2]]
}
}
\references{
Plecko, D. & Meinshausen, N. (2019). Fair Data Adaptation with Quantile Preservation \cr
}
\author{
Drago Plecko
}
