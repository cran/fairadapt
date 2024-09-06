## ---- setup, include = FALSE----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(fairadapt)
library(ggplot2)
library(ggraph)
library(ranger)
library(microbenchmark)
library(xtable)

options(
  prompt = "R> ",
  continue = "+ ",
  width = 70,
  useFancyQuotes = FALSE,
  tinytex.verbose = TRUE,
  kableExtra.latex.load_packages = FALSE
)

quick_build <- !identical(Sys.getenv("NOT_CRAN"), "true") ||
  isTRUE(as.logical(Sys.getenv("CI"))) ||
  !identical(Sys.getenv("FAIRADAPT_VIGNETTE_QUICK_BUILD"), "false")

is_on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
run_sim <- FALSE

## ---- basic---------------------------------------------------------
n_samp <- 500

uni_dat <- data("uni_admission", package = "fairadapt")
uni_dat <- uni_admission[seq_len(2 * n_samp), ]

head(uni_dat)

uni_trn <- head(uni_dat, n = n_samp)
uni_tst <- tail(uni_dat, n = n_samp)

uni_dim <- c(       "gender", "edu", "test", "score")
uni_adj <- matrix(c(       0,     1,      1,       0,
                            0,     0,      1,       1,
                            0,     0,      0,       1,
                            0,     0,      0,       0),
                  ncol = length(uni_dim),
                  dimnames = rep(list(uni_dim), 2),
                  byrow = TRUE)

set.seed(2022)
basic <- fairadapt(score ~ ., train.data = uni_trn,
                    test.data = uni_tst, adj.mat = uni_adj,
                    prot.attr = "gender")

basic

## ----fairadapt-summary----------------------------------------------
summary(basic)

## ----adapted-data-example-------------------------------------------
head(adaptedData(basic, train = FALSE))

## ---- graph-model---------------------------------------------------
uni_graph <- graphModel(uni_adj)

## ---- graph-plot, echo = FALSE, fig.width = 6, fig.height = 3, fig.cap = "The underlying graphical model corresponding to the university admission example (also shown in Figure \\ref{fig:uni-adm}).", out.width = "60%"----

set.seed(11)

ggraph(graphModel(uni_adj), "igraph", algorithm = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(4, "mm"), angle = 15,
                               type = "closed"),
                 end_cap = circle(8, "mm"),
                 color = "grey20") +
  geom_node_point(color = "grey80", size = 21) +
  geom_node_text(aes(x = x, y = y, label = name), size = 5) +
  theme_bw() +
  theme(plot.margin = margin(30, 30, 30, 30), panel.border = element_blank(),
        panel.background = element_blank(), axis.title = element_blank(),
        axis.text = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank()) +
  coord_cartesian(clip = "off")

## ----benchmark-quant-methods, echo = FALSE, results = "asis"--------

linebreak <- function(..., align = c("l", "c", "r"), linebreaker = "\n") {

  x <- c(...)

  ifelse(
    grepl(linebreaker, x, fixed = TRUE),
    paste0("\\makecell[", match.arg(align), "]{",
           gsub(linebreaker, "\\\\", x, fixed = TRUE), "}"),
    x
  )
}

bm_cache <- system.file("extdata", "bm_quant.rds", package = "fairadapt")

if (!run_sim && file.exists(bm_cache)) {

  bmark <- readRDS(bm_cache)

} else {

  nsamps <- c(200L, 500L)

  bmark <- lapply(
    c(rangerQuants, mcqrnnQuants, linearQuants),
    function(quant.method) {
      lapply(
        nsamps, function(nsamp) {
          tim <- microbenchmark(
            fairadapt(score ~ ., train.data = uni_admission[seq_len(nsamp), ],
                      test.data = uni_admission[seq.int(nsamp + 1 , 2 * nsamp), ],
                      adj.mat = uni_adj, prot.attr = "gender",
                      quant.method = quant.method), times = 5L
          )$time
          round(mean(tim) / 10^9, digits = 1L)
        }
      )
    }
  )

  bmark <- do.call(cbind, bmark)

  colnames(bmark) <- c("Random forest", "Neural network", "Linear regression")
  rownames(bmark) <- as.character(nsamps)

  saveRDS(bmark, file.path("..", "inst", "extdata", "bm_quant.rds"))
}

rownames(bmark) <- paste0("$T_{\\text{uni}}(", rownames(bmark), ")$")

tbl <- data.frame(
  linebreak(
    "\\pkg{ranger}", "\\code{rangerQuants}", "$O(np\\log n)$",
    "$ntrees = 500$\n$mtry = \\sqrt{p}$"
  ),
  linebreak(
    "\\pkg{qrnn}", "\\code{mcqrnnQuants}", "$O(npn_{\\text{epochs}})$",
    "1 hidden layer\nfully connected\nfeed-forward\nnetwork"
  ),
  linebreak(
    "\\pkg{quantreg}", "\\code{linearQuants}", "$O(p^2n)$",
    "\\code{\"br\"} method of\nBarrodale and\nRoberts used for\nfitting"
  )
)

colnames(tbl) <- colnames(bmark)
rownames(tbl) <- linebreak(
  "\\proglang{R}-package", "\\texttt{quant.method}", "Complexity",
  "Default\nparameters"
)

tbl <- rbind(tbl, bmark)

capt <- paste(
  "Summary table of different quantile regression methods. $n$ is the number",
  "of samples, $p$ number of covariates, $n_{\\text{epochs}}$ number of",
  "training epochs for the neural network. $T_{\\text{uni}}(n)$ denotes the",
  "runtime of different methods on the university admission dataset, with $n$",
  "training and $n$ testing samples. The runtimes were obtained on a system", 
  "with Intel Core i7-8750H CPU @ 2.2GHz running MacOS Big Sur 11.6.",
  "The version of \\proglang{R} was 4.2.0 \"Vigorous Calisthenics\" with \\pkg{quantreg}", 
  "version 5.93, \\pkg{ranger} version 0.13.1, and \\pkg{mcqrnn} version 2.0.5."
)

print(
  xtable(tbl, caption = capt, label = "tab:qmethods",
         align = rep("l", ncol(tbl) + 1L)),
  booktabs = TRUE,
  table.placement = "t",
  sanitize.text.function = identity,
  comment = FALSE
)

## ---- quantFit------------------------------------------------------
set.seed(22)
fit_qual <- fairadapt(score ~ ., train.data = uni_trn,
                      adj.mat = uni_adj, prot.attr = "gender",
                      eval.qfit = 3L)

quantFit(fit_qual)

## ---- fairtwin-uni--------------------------------------------------
ft_basic <- fairTwins(basic, train.id = seq_len(n_samp))
head(ft_basic, n = 3)

## ---- compas--------------------------------------------------------
cmp_dat <- data("compas", package = "fairadapt")
cmp_dat <- get(cmp_dat)

cmp_mat <- matrix(0, nrow = ncol(cmp_dat), ncol = ncol(cmp_dat),
                  dimnames = list(names(cmp_dat), names(cmp_dat)))

cmp_mat[c("race", "sex", "age"),
        c("juv_fel_count", "juv_misd_count",
          "juv_other_count", "priors_count",
          "c_charge_degree", "two_year_recid")] <- 1
cmp_mat[c("juv_fel_count", "juv_misd_count", "juv_other_count"),
        c("priors_count", "c_charge_degree", "two_year_recid")] <- 1
cmp_mat["priors_count", c("c_charge_degree", "two_year_recid")] <- 1
cmp_mat["c_charge_degree", "two_year_recid"] <- 1

head(cmp_dat)

## ---- compas-boot-quick, eval = quick_build, echo = quick_build-----
#  cmp_trn <- tail(cmp_dat, n = 100L)
#  cmp_tst <- head(cmp_dat, n = 100L)
#  
#  n_itr <- 3L
#  
#  set.seed(2022)
#  fa_boot_fin <- fairadaptBoot(two_year_recid ~ ., "race", cmp_mat,
#                               cmp_trn, cmp_tst, rand.mode = "finsamp",
#                               n.boot = n_itr)

## ---- compas-boot-slow, eval = !quick_build, echo = !quick_build----
cmp_trn <- tail(cmp_dat, n = 6000L)
cmp_tst <- head(cmp_dat, n = 1214L)

n_itr <- 50L

set.seed(2022)
fa_boot_fin <- fairadaptBoot(two_year_recid ~ ., "race", cmp_mat,
                             cmp_trn, cmp_tst, rand.mode = "finsamp",
                             n.boot = n_itr)

## ---- compas-boot-quant---------------------------------------------
set.seed(2022)
fa_boot_quant <- fairadaptBoot(two_year_recid ~ ., "race", cmp_mat,
                               cmp_trn, cmp_tst, rand.mode = "quant",
                               n.boot = n_itr)

## ---- compas-forest-------------------------------------------------
fit_rf <- function(x) {
  ranger(factor(two_year_recid) ~ ., cmp_trn[x, ], probability = TRUE)
}

extract_pred <- function(x) x$predictions[, 2L]

set.seed(2022)
cmp_rf <- lapply(fa_boot_fin$boot.ind, fit_rf)

pred_fin <- Map(predict, cmp_rf, adaptedData(fa_boot_fin, train = FALSE)) 
pred_fin <- do.call(cbind, lapply(pred_fin, extract_pred))

pred_quant <- Map(predict, cmp_rf, adaptedData(fa_boot_quant, train = FALSE))
pred_quant <- do.call(cbind, lapply(pred_quant, extract_pred))

## ---- decision-maker-1----------------------------------------------
jac_frm <- function(x, modes = "single") {

  jac <- function(a, b) {
    intersection <- length(intersect(a, b))
    union <- length(a) + length(b) - intersection
    intersection / union
  }

  res <- lapply(
    seq(quantile(x[, 1L], 0.05), quantile(x[, 1L], 0.95), 0.01),
    function(tsh) {

      ret <- replicate(100L, {
        col <- sample(ncol(x), 2L)
        jac(which(x[, col[1L]] > tsh), which(x[, col[2L]] > tsh))
      })

      data.frame(tsh = tsh, y = mean(ret), sd = sd(ret),
                 mode = modes)
    }
  )

  do.call(rbind, res)
}

jac_df <- rbind(jac_frm(pred_fin, "Finite Sample"),
                jac_frm(pred_quant, "Quantiles"))

## ---- decision-maker-2----------------------------------------------
ord_ind <- function(x, modes = "single") {

  res <- replicate(5000L, {
    row <- sample(nrow(x), 2)
    ord <- mean(x[row[1], ] > x[row[2], ])
    ord^2 + (1 - ord)^2
  })

  data.frame(res = res, mode = modes)
}

ord_df <- rbind(ord_ind(pred_fin, "Finite Sample"),
                ord_ind(pred_quant, "Quantiles"))

## ---- decision-maker-3----------------------------------------------
inv_frm <- function(x, modes = "single") {

  gt <- function(x) x[1L] > x[2L]

  res <- replicate(100L, {
    col <- sample(ncol(x), 2L)
    prm <- order(x[, col[2L]][order(x[, col[1L]])])
    sum(combn(prm, 2L, gt)) / choose(length(prm), 2L)
  })

  data.frame(res = res, mode = modes)
}

inv_df <- rbind(inv_frm(pred_fin, "Finite Sample"),
                inv_frm(pred_quant, "Quantiles"))

## ---- decision-maker-4----------------------------------------------
prb_frm <- function(x, modes = "single") {
  qnt <- apply(x, 1L, quantile, probs = c(0.05, 0.95))
  data.frame(width = qnt[2L, ] - qnt[1L, ], mode = modes)
}

prb_df <- rbind(prb_frm(pred_fin, "Finite Sample"),
                prb_frm(pred_quant, "Quantiles"))

## ---- compas-dm, echo = FALSE, fig.width = 8, fig.height = 6, warning = FALSE, fig.cap = "Analyzing uncertainty of predictions in the COMPAS dataset from decision-maker's point of view. Panel A shows how the Jaccard similarity of two repetitions varies depending on the decision threshold. Panel B shows the cumulative distribution of the random variable that indicates whether two randomly selected individuals preserve order (in terms of predicted probabilities) in bootstrap repetitions. Panel C shows the density of the normalized inversion number of between predicted probabilities in bootstrap repetitions. Panel D shows the cumulative distribution function of the 95\\% confidence interval (CI) width for the predicted probability of different individuals.", out.width = "100%"----

p1 <- ggplot(jac_df, aes(x = tsh, y = y, color = mode)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = y - sd, ymax = y + sd, fill = mode, color = mode),
                alpha = 0.3) +
  theme_bw() +
  xlab("Decision threshold") +
  ylab("Jaccard similarity") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.3),
    legend.box.background = element_rect()
  ) +
  scale_color_discrete(labels = c(" \"finsamp\" ", " \"quant\" "),
                      name = "rand.mode") +
  scale_fill_discrete(labels = c(" \"finsamp\" ", " \"quant\" "),
                      name = "rand.mode")

p2 <- ggplot(ord_df, aes(x = res, color = mode)) +
  stat_ecdf() +
  theme_bw() +
  xlab("Probability of preserving ordering") +
  ylab("Cumulative proportion") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.7),
    legend.box.background = element_rect()
  ) +
  scale_color_discrete(labels = c(" \"finsamp\" ", " \"quant\" "),
                       name = "rand.mode")

p3 <- ggplot(inv_df, aes(x = res, fill = mode)) +
  geom_density(alpha = 0.3) +
  xlim(0, 0.25) +
  theme_bw() +
  xlab("Normalized inversion number") +
  ylab("Density") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.7),
    legend.box.background = element_rect()
  ) +
  scale_fill_discrete(labels = c(" \"finsamp\" ", " \"quant\" "),
                      name = "rand.mode")

p4 <- ggplot(prb_df, aes(x = width, color = mode)) +
  stat_ecdf() +
  theme_bw() +
  xlab("95% CI width") +
  ylab("Cumulative probability") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.7),
    legend.box.background = element_rect()
  ) +
  scale_x_reverse() +
  scale_color_discrete(labels = c(" \"finsamp\" ", " \"quant\" "),
                       name = "rand.mode")

cowplot::plot_grid(
  p1, p2, p3, p4, ncol = 2L, labels = LETTERS[1:4]
)

## ---- indiv-uncq----------------------------------------------------
ind_prb <- data.frame(
  prob = as.vector(t(pred_quant[seq_len(3), ])),
  individual = rep(c(1, 2, 3), each = fa_boot_quant$n.boot)
)

## ---- compas-indiv, echo = FALSE, fig.width = 7, fig.height = 3.5, fig.cap = "Analyzing the spread of individual predictions in the COMPAS dataset, resulting from different bootstrap repetitions.", out.width = "100%"----
ggplot(ind_prb, aes(x = prob, group = individual,
                    fill = factor(individual))) +
  geom_density(alpha = 0.2) +
  scale_fill_discrete(labels = paste0("#", seq_len(3)),
                      name = "Individual") +
  xlab("Probability Estimate") +
  ylab("Density") +
  theme_bw() +
  xlim(0, 1) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.75),
    legend.box.background = element_rect()
  )

## ---- load-census---------------------------------------------------
gov_dat <- data("gov_census", package = "fairadapt")
gov_dat <- get(gov_dat)

dem <- c("age", "race", "hispanic_origin", "citizenship",
         "nativity", "economic_region")
fam <- c("marital", "family_size", "children")
edu <- c("education_level", "english_level")
occ <- c("hours_worked", "weeks_worked", "occupation",
         "industry")

prt <- "sex"
res <- "salary"

## ---- census-adj----------------------------------------------------
cols <- c(dem, fam, edu, occ, prt, res)

gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
                  dimnames = rep(list(cols), 2))
gov_cfd <- gov_adj

gov_adj[dem, c(fam, edu, occ, res)] <- 1
gov_adj[fam, c(     edu, occ, res)] <- 1
gov_adj[edu, c(          occ, res)] <- 1
gov_adj[occ,                  res ] <- 1

gov_adj[prt, c(fam, edu, occ, res)] <- 1

gov_cfd[prt, dem] <- 1
gov_cfd[dem, prt] <- 1

gov_grph <- graphModel(gov_adj, gov_cfd)

## ---- census-graph, echo = FALSE, fig.height = 9, fig.width = 9, fig.cap = "Full causal graph for the government census dataset, expanding the grouped view presented in Figure \\ref{fig:census-tikz}. \\textit{Demographic} features include age (\\textbf{ag}), race (\\textbf{ra}), whether an employee is of Hispanic origin (\\textbf{hi}), is US citizen (\\textbf{ci}), whether the citizenship is native (\\textbf{na}), alongside the corresponding economic region (\\textbf{ec}). \\textit{Familial} features are marital status (\\textbf{ma}), family size (\\textbf{fa}) and number of children (\\textbf{ch}), \\textit{educational} features include education (\\textbf{ed}) and English language levels (\\textbf{en}), and \\textit{occupational} features, weekly working hours (\\textbf{ho}), yearly working weeks (\\textbf{we}), job (\\textbf{oc}) and industry identifiers (\\textbf{in}). Finally, the yearly salary (\\textbf{sa}) is used as the \\textit{response} variable and employee sex (\\textbf{se}) as the \\textit{protected} attribute variable.", out.width = "90%"----
set.seed(11)
gov_tmp <- graphModel(
  `dimnames<-`(gov_adj, lapply(dimnames(gov_adj), substr, 1L, 2L)),
  `dimnames<-`(gov_cfd, lapply(dimnames(gov_cfd), substr, 1L, 2L))
)

grp <- list(dem, fam, edu, occ, prt, res)
grp <- `names<-`(
  rep(c("demographic", "familial", "educational", "occupational",
        "protected", "response"), lengths(grp)),
  substr(unlist(grp), 1L, 2L)
)

igraph::V(gov_tmp)$color <- grp[names(igraph::V(gov_tmp))]

gov_tmp <- igraph::delete_edges(
  gov_tmp, which(igraph::E(gov_tmp)$lty == "blank")
)

lty_selector <- function(lty) {
  function(layout) {
    get_all <- get_edges()
    edges <- get_all(layout)
    res <- edges[edges$lty == lty, ]
    res
  }
}

ggraph(gov_tmp, "igraph", algorithm = "fr") +
  geom_edge_arc(data = lty_selector("dashed"),
                arrow = arrow(length = unit(4, "mm"), angle = 15,
                              ends = "both", type = "closed"),
                start_cap = circle(6.5, "mm"),
                end_cap = circle(6.5, "mm"),
                strength = 0.25,
                color = "grey20",
                linetype = "dashed") +
  geom_edge_link(data = lty_selector("solid"),
                 arrow = arrow(length = unit(4, "mm"), angle = 15,
                               type = "closed"),
                 end_cap = circle(6.5, "mm"),
                 color = "grey20",
                 linetype = "solid") +
  geom_node_point(aes(color = color), size = 15) +
  geom_node_text(aes(x = x, y = y, label = name), size = 5) +
  scale_color_discrete(name = "Grouping") +
  theme_bw() +
  theme(plot.margin = margin(30, 30, 30, 30), legend.position = "bottom",
        panel.background = element_blank(), axis.title = element_blank(),
        axis.text = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank()) +
  coord_cartesian(clip = "off")

## ---- log-sub-quick, eval = quick_build, echo = quick_build---------
#  n_samp <- 750
#  n_pred <- 5

## ---- log-sub-slow, eval = !quick_build, echo = !quick_build--------
n_samp <- 30000
n_pred <- 5

## ---- census-adapt--------------------------------------------------
gov_dat$salary <- log(gov_dat$salary)

gov_trn <- head(gov_dat, n = n_samp)
gov_prd <- tail(gov_dat, n = n_pred)

set.seed(22)
gov_ada <- fairadapt(salary ~ ., train.data = gov_trn,
                     adj.mat = gov_adj, cfd.mat = gov_cfd,
                     prot.attr = prt)

## ---- census-vis, echo = FALSE, fig.width = 7, fig.height = 3, fig.cap = "Visualization of salary densities grouped by employee sex, before (panel A) and after adaptation (panel B). Panel A indicates a shift towards higher values for male employees. In panel B, after the data is transformed, the gap between groups is reduced.", cache = TRUE----

p1 <- ggplot(gov_trn, aes(x = salary, fill = sex)) +
  geom_density(alpha = 0.4)  +
  theme_bw()

p2 <- autoplot(gov_ada, when = "after") +
  theme_bw() + ggtitle(NULL)

legend_join <- cowplot::get_plot_component(
  p1 + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"),
    "guide-box-bottom", return_all = TRUE
)

panels <- cowplot::plot_grid(
  p1 + theme(legend.position = "none"), 
  p2 + theme(legend.position = "none"), labels = LETTERS[1:2]
)
cowplot::plot_grid(panels, legend_join, ncol = 1, rel_heights = c(1, .08))

## ---- census-predict------------------------------------------------
set.seed(2022)
gov_prd_ada <- predict(gov_ada, newdata = gov_prd)
gov_prd_ada[, c("sex", "age", "education_level", "salary")]

## ---- census-twins--------------------------------------------------
fairTwins(gov_ada, train.id = 1:5,
          cols = c("sex", "age", "salary"))

## ---- res-uni-------------------------------------------------------
res_basic <- fairadapt(score ~ ., train.data = uni_trn, 
                       test.data = uni_tst, adj.mat = uni_adj, 
                       prot.attr = "gender", res.vars = "test")
summary(res_basic)

## ---- res-assign----------------------------------------------------
uni_res <- graphModel(uni_adj, res.vars = "test")

## ---- res-graph, echo = FALSE, fig.width = 6, fig.height = 3, fig.cap = "Visualization of the causal graph corresponding to the university admissions example introduced in Section \\ref{methodology} with the variable \\texttt{test} chosen as a \\textit{resolving variable} and therefore highlighted in red.", out.width = "60%"----

set.seed(11)

ggraph(graphModel(uni_adj, res.vars = "test"), "igraph",
       algorithm = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(4, "mm"), angle = 15,
                               type = "closed"),
                 end_cap = circle(8, "mm"),
                 color = "grey20") +
  geom_node_point(aes(color = color), size = 21, show.legend = FALSE) +
  geom_node_text(aes(x = x, y = y, label = name), size = 5) +
  theme_bw() +
  theme(plot.margin = margin(30, 30, 30, 30), panel.border = element_blank(),
        panel.background = element_blank(), axis.title = element_blank(),
        axis.text = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank()) +
  coord_cartesian(clip = "off")

## ---- semi-markov-uni-----------------------------------------------
uni_cfd <- matrix(0, nrow = nrow(uni_adj), ncol = ncol(uni_adj),
                  dimnames = dimnames(uni_adj))

uni_cfd["test", "score"] <- 1
uni_cfd["score", "test"] <- 1

semi <- fairadapt(score ~ ., train.data = uni_trn,
                   test.data = uni_tst, adj.mat = uni_adj,
                   cfd.mat = uni_cfd, prot.attr = "gender")

## ---- semi-graph, echo = FALSE, fig.width = 6, fig.height = 3, fig.cap = "Visualization of the causal graphical model also shown in Figure \\ref{fig:semi-markov}, obtained when passing a confounding matrix indicating a bidirected edge between vertices \\texttt{test} and \\texttt{score} to \\texttt{fairadapt()}. The resulting Semi-Markovian model can also be handled by \\texttt{fairadapt()}, extending the basic Markovian formulation introduced in Section \\ref{markovian-scm-formulation}.", out.width = "60%"----

set.seed(17)

ggraph(semi$graph, "igraph", algorithm = "fr") +
  geom_edge_arc(data = lty_selector("dashed"),
                arrow = arrow(length = unit(4, "mm"), angle = 15,
                              ends = "both", type = "closed"),
                start_cap = circle(8, "mm"),
                end_cap = circle(8, "mm"),
                strength = 0.25,
                color = "grey20",
                linetype = "dashed") +
  geom_edge_link(data = lty_selector("solid"),
                 arrow = arrow(length = unit(4, "mm"), angle = 15,
                               type = "closed"),
                 end_cap = circle(8, "mm"),
                 color = "grey20",
                 linetype = "solid") +
  geom_node_point(color = "grey80", size = 21) +
  geom_node_text(aes(x = x, y = y, label = name), size = 5) +
  theme_bw() +
  theme(plot.margin = margin(30, 30, 30, 30), panel.border = element_blank(),
        panel.background = element_blank(), axis.title = element_blank(),
        axis.text = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank()) +
  coord_cartesian(clip = "off")

## ----top-ord--------------------------------------------------------
set.seed(2022)
top_ord <- fairadapt(score ~ ., train.data = uni_trn, test.data = uni_tst, 
                      top.ord = c("gender", "edu", "test", "score"),
                      prot.attr = "gender")

summary(top_ord)

## ----session-info, include = FALSE----------------------------------
sessionInfo()

