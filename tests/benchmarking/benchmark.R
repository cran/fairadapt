library(microbenchmark)
library(fairadapt)
library(igraph)

adj.mat <- array(0, dim = c(4, 4))
colnames(adj.mat) <- rownames(adj.mat) <-
  c("gender", "edu", "test", "score")

adj.mat["gender", c("edu", "test")] <-
  adj.mat["edu", c("test", "score")] <-
  adj.mat["test", "score"] <- 1L


nsamp <- 500L

for (quant.method in c("forest", "linear", "nn")) {

  print(
    microbenchmark(
      FA <- fairadapt(score ~ ., train.data = uni_admission[1:nsamp, ], test.data = uni_admission[(nsamp+1):(2*nsamp), ],
        adj.mat = adj.mat, protect.A = "gender", res.vars = NULL,
        visualize.graph = F, quant.method = quant.method), times = 1L
    )
  )

}
