library(fairadapt)
library(mvtnorm)
pred_a123 <- function(adj.mat, cov.mat, phi, n = 5000, res = NULL, CFD = T, ...) {

  c_SEM <- function(A, eps, res) {

    L <- list()
    X <- list()

    for (i in 1:2) {

      for(j in 1:ncol(eps)) {

        if(!(paste0("X", j) %in% res) | i == 1)
          X[[j]] <- phi[[j]](A[[i]], X, eps[, j])

      }


      df <- data.frame(A = A[[i]], Reduce(cbind, X))
      names(df) <- c("A", paste0("X", 1:ncol(eps)))
      L[[i]] <- df

    }

    L

  }

  cfd.mat <- ( (cov.mat != 0) + diag(ncol(cov.mat)) ) * CFD

  set.seed(202211)
  eps <- 1 * rmvnorm(n, mean = rep(0, length(phi)), sigma = cov.mat[-1, -1] +
                       diag(length(phi)))
  A <- rbinom(n, 1, 0.5)

  train.idx <- seq_len(n / 2)
  train <- c_SEM(list(A, 0), eps, res)

  test <- train[[1]][-train.idx, ]
  cf <- train[[2]][-train.idx, ]
  train <- train[[1]][train.idx, ]


  # do adaptation
  fair.sep <- fairadapt::fairadapt(as.formula(paste0("X", length(phi), " ~ .")),
                                   train.data = train, test.data = NULL,
                                   adj.mat = adj.mat, cfd.mat = cfd.mat,
                                   protect.A = "A", res.vars = res,
                                   ...)

  fair.sep <- predict(fair.sep, newdata = test)

  fair.join <- fairadapt::fairadapt(as.formula(paste0("X", length(phi), " ~ .")),
                                   train.data = train, test,
                                   adj.mat = adj.mat, cfd.mat = cfd.mat,
                                   protect.A = "A", res.vars = res,
                                   ...)[["adapt.test"]]



  par(mfrow = c(length(phi) - 1L, 2L))

  idx <- test$A == 1

  for(i in paste0("X", seq_len(length(phi) - 1L))) {

    if (i == "X4") browser()

    MSE1 <- 100 * mean((fair.join[idx, i] - cf[idx, i])^2)
    MSE2 <- 100 * mean((fair.sep[idx, i] - cf[idx, i])^2)

    plot(fair.join[idx, i], cf[idx, i], pch = 19, xlab = "True", ylab = "Ctf",
         main = paste("Joint learning: ", i, round(MSE1, 2)))
    abline(0, 1, col = "red", lwd = 3)

    plot(fair.sep[idx, i], cf[idx, i], pch = 19, xlab = "Fair", ylab = "Ctf",
         main = paste("Separated learning: ", i, ":", round(MSE2, 2)))
    abline(0, 1, col = "red", lwd = 3)

  }

}

f <- list(
  function(A, X, eps1) 2*A - 1 + eps1,
  function(A, X, eps2) 2*A - 1 + 1/25 * (X[[1]]+5)^2 + eps2,
  function(A, X, eps3) 1/4 * (X[[1]]+5) * (X[[2]]+6) + eps3,
  function(A, X, eps4) 0 * A + 0 / 2 * X[[1]] + 1 * X[[2]]*log((X[[2]])+50) +
    1 / 50 * X[[3]]^3 + eps4
)

adj.mat <- array(0, dim = c(length(f) + 1, length(f) + 1))
colnames(adj.mat) <- rownames(adj.mat) <- c("A", paste0("X", 1:length(f)))
adj.mat["A", "X1"] <-
  adj.mat["A", "X2"] <-
  adj.mat["X1", "X2"] <-
  adj.mat["X1", "X3"] <-
  adj.mat["X2", "X3"] <-
  adj.mat["X3", "X4"] <-
  adj.mat["X2", "X4"] <-
  1

cov.mat <- adj.mat
cov.mat[, ] <- 0

#install()
#reload()

pred_a123(adj.mat, cov.mat, phi = f, CFD = T, n=2000,
          quant.method = fairadapt:::rangerQuants)

