## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(tinytex.verbose = TRUE)

library(fairadapt)
library(igraph)
library(data.table)
library(ggplot2)

## ----min-example, include=T---------------------------------------------------
uni.adj.mat <- array(0, dim = c(4, 4))
colnames(uni.adj.mat) <- rownames(uni.adj.mat) <-
  c("gender", "edu", "test", "score")

uni.adj.mat["gender", c("edu", "test")] <-
  uni.adj.mat["edu", c("test", "score")] <-
  uni.adj.mat["test", "score"] <- 1L

nsamp <- 200

FA.basic <- fairadapt(score ~ .,
  train.data = uni_admission[1:nsamp, ],
  test.data = uni_admission[(nsamp+1):(2*nsamp), ],
  adj.mat = uni.adj.mat, prot.attr = "gender", res.vars = NULL,
  visualize.graph = F, quant.method = fairadapt:::rangerQuants)

FA.basic

## ----mat, include=T, out.width="200px", out.height="200px"--------------------
toy.graph <- graphModel(uni.adj.mat)
plot(toy.graph, vertex.size = 40, vertex.label.cex = 0.5,
  vertex.label.color = "black")

## ----fairtwin-uni, include = T------------------------------------------------
fairTwins(FA.basic, train.id = seq.int(1L, 5L, 1L))

## ----load-census, include=T---------------------------------------------------
dat <- gov_census
print(head(dat))

# group the columns
prot.attr <- "sex"
dmgraph <- c("age", "race", "hispanic_origin", "citizenship", "nativity",
  "economic_region")
fam <- c("marital", "family_size", "children")
edu <- c("education_level", "english_level")
work <- c("hours_worked", "weeks_worked", "occupation", "industry")
out <- "salary"

## ----census-graph, include=T, fig.height=5------------------------------------
col.names <- c(prot.attr, dmgraph, fam, edu, work, out)

adj.mat <- cfd.mat <- array(0, dim = c(length(col.names), length(col.names)))
colnames(adj.mat) <- rownames(adj.mat) <-
  colnames(cfd.mat) <- rownames(cfd.mat) <-
  col.names

adj.mat[prot.attr, c(fam, edu, work, out)] <-
  adj.mat[dmgraph, c(fam, edu, work, out)] <-
  adj.mat[fam, c(edu, work, out)] <-
  adj.mat[edu, c(work, out)] <-
  adj.mat[work, out] <-
  cfd.mat[prot.attr, dmgraph] <- cfd.mat[dmgraph, prot.attr] <- 1L

census.graph <- graphModel(adj.mat, cfd.mat)
plot(census.graph, vertex.size = 20, vertex.label.cex = 0.5,
  vertex.label.color = "black")

## ----log-and-graph, include=T, fig.height=3-----------------------------------
# log-transform the salaries
dat$salary <- log(dat$salary)

# plot density before adaptation
nsamples <- 2000

ggplot(dat[1:nsamples], aes(x = salary, fill = sex)) +
  geom_density(alpha = 0.4)  + theme_minimal() +
  ggtitle("Salary density by gender")

## ----census-adapt, include=T--------------------------------------------------
FA.govcensus <- fairadapt(salary ~ ., train.data = dat[1:nsamples],
                          adj.mat = adj.mat, prot.attr = prot.attr,
                          visualize.graph = F)

## ----vis-adapt, include=T, fig.height=3---------------------------------------
autoplot(FA.govcensus, when = "after") +
  ggtitle("Adapted salary density by gender")

## ----census-predict, include=T------------------------------------------------
new.test <- dat[seq.int(nsamples + 1L, nsamples + 10L, 1L)]
adapt.test <- predict(FA.govcensus, newdata = new.test)
head(adapt.test)

## ----census-twins, include=T--------------------------------------------------
inspect.cols <- c("sex", "age", "education_level", "salary")
fairTwins(FA.govcensus, train.id = 1:5, cols = inspect.cols)

## ----res-uni-admission, include=T---------------------------------------------
FA.resolving <- fairadapt(score ~ .,
  train.data = uni_admission[1:nsamp, ],
  test.data = uni_admission[(nsamp+1):(2*nsamp), ],
  adj.mat = uni.adj.mat, prot.attr = "gender", res.vars = "test",
  visualize.graph = F)

FA.resolving

## ----resolving-graph, include = T, out.width="200px", out.height="200px"------
plot(graphModel(uni.adj.mat, res.vars = "test"),
  vertex.size = 40, vertex.label.cex = 0.5,
  vertex.label.color = "black")

## ----semi-markov-uni, include=T-----------------------------------------------
uni.cfd.mat <- array(0, dim = c(4, 4))
colnames(uni.cfd.mat) <- rownames(uni.cfd.mat) <- colnames(uni.adj.mat)

uni.cfd.mat["test", "score"] <- uni.cfd.mat["score", "test"] <- 1L
FA.semimarkov <- fairadapt(score ~ .,
  train.data = uni_admission[1:nsamp, ],
  test.data = uni_admission[(nsamp+1):(2*nsamp), ],
  adj.mat = uni.adj.mat, cfd.mat = uni.cfd.mat, prot.attr = "gender",
  visualize.graph = F)

## ----graph-semimarkov, include=T, out.width="200px", out.height="200px"-------
plot(FA.semimarkov, graph = T, vertex.size = 40,
  vertex.label.cex = 0.5, vertex.label.color = "black")

## ----vis-diff, include=T------------------------------------------------------
plot(
  FA.basic[["adapt.train"]][["score"]],
  FA.semimarkov[["adapt.train"]][["score"]], pch = 19,
  main = "Markovian vs. Semi-Markovian case on uni_admission",
  xlab = "Markov adaptation score",
  ylab = "Semi-Markov adaptation score"
)
abline(0, 1, col = "red", lwd = 2)

