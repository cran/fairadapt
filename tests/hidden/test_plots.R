uni.adj.mat <- array(0, dim = c(4, 4))
colnames(uni.adj.mat) <- rownames(uni.adj.mat) <-
  c("gender", "edu", "test", "score")

uni.adj.mat["gender", c("edu", "test")] <-
  uni.adj.mat["edu", c("test", "score")] <-
  uni.adj.mat["test", "score"] <- 1L

nsamp <- 500

uni_adm <- uni_admission
uni_adm$score <- as.integer(uni_adm$score > 0)

FA.basic <- fairadapt(score ~ .,
  train.data = uni_adm[1:nsamp, ],
  test.data = uni_adm[(nsamp+1):(2*nsamp), ],
  adj.mat = uni.adj.mat, protect.A = "gender", res.vars = NULL,
  visualize.graph = F, quant.method = "forest")

FA.basic

plot(FA.basic, graph = F, when = "both")


