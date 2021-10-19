## ---- setup, include = FALSE----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(fairadapt)
library(data.table)
library(ggplot2)
library(ggraph)
options(tinytex.verbose = TRUE)

options(
  prompt = "R> ",
  continue = "+ ",
  width = 70,
  useFancyQuotes = FALSE
)

## ---- basic---------------------------------------------------------
n_samp <- 200

uni_dat <- data("uni_admission", package = "fairadapt")
uni_dat <- uni_admission[seq_len(2 * n_samp), ]

head(uni_dat)

uni_trn <- head(uni_dat, n = n_samp)
uni_tst <- tail(uni_dat, n = n_samp)

uni_dim <- c(       "gender", "edu", "test", "score")
uni_adj <- matrix(c(       0,     0,      0,       0,
                            1,     0,      0,       0,
                            1,     1,      0,       0,
                            0,     1,      1,       0),
                  ncol = length(uni_dim),
                  dimnames = rep(list(uni_dim), 2))

basic <- fairadapt(score ~ ., train.data = uni_trn,
                    test.data = uni_tst, adj.mat = uni_adj,
                    prot.attr = "gender")

basic

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

## ---- fairtwin-uni--------------------------------------------------
ft_basic <- fairTwins(basic, train.id = seq_len(n_samp))
head(ft_basic, n = 3)

## ---- load-census---------------------------------------------------
gov_dat <- data("gov_census", package = "fairadapt")
gov_dat <- get(gov_dat)

head(gov_dat)

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
  scale_edge_linetype_manual(values = c(dashed = "dashed", solid = "solid")) +
  scale_color_discrete(name = "Grouping") +
  theme_bw() +
  theme(plot.margin = margin(30, 30, 30, 30), legend.position = "bottom",
        panel.background = element_blank(), axis.title = element_blank(),
        axis.text = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank()) +
  coord_cartesian(clip = "off")

## ---- log-sub-------------------------------------------------------
gov_dat$salary <- log(gov_dat$salary)

n_samp <- 30000
n_pred <- 5

gov_trn <- head(gov_dat, n = n_samp)
gov_prd <- tail(gov_dat, n = n_pred)

## ---- before-adapt, echo = FALSE, fig.height = 3, fig.cap = "Visualization of salary densities grouped by employee sex, indicating a shift to higher values for male employees. This uses the US government-census dataset and shows the situation before applying fair data adaption, while Figure \\ref{fig:vis-adapt} presents transformed salary data."----
ggplot(gov_trn, aes(x = salary, fill = sex)) +
  geom_density(alpha = 0.4)  +
  theme_bw() +
  ggtitle("Salary density by gender")

## ---- census-adapt--------------------------------------------------
gov_ada <- fairadapt(salary ~ ., train.data = gov_trn,
                     adj.mat = gov_adj, prot.attr = prt)

## ---- vis-adapt, fig.height = 3, fig.cap = "The salary gap between male and female employees of the US government according to the government-census dataset is clearly reduced when comparing raw data (see Figure \\ref{fig:before-adapt}) to transformed salary data as yielded by applying fair data adaption using employee sex as the protected attribute and assuming a causal graph as shown in Figure \\ref{fig:census-graph}."----
autoplot(gov_ada, when = "after") +
  theme_bw() +
  ggtitle("Adapted salary density by gender")

## ---- census-predict------------------------------------------------
predict(gov_ada, newdata = gov_prd)

## ---- census-twins--------------------------------------------------
fairTwins(gov_ada, train.id = 1:5,
          cols = c("sex", "age", "education_level", "salary"))

## ---- res-uni-------------------------------------------------------
fairadapt(score ~ ., train.data = uni_trn, test.data = uni_tst,
          adj.mat = uni_adj, prot.attr = "gender", res.vars = "test")

## ---- res-assign----------------------------------------------------
uni_res <- graphModel(uni_adj, res.vars = "test")

## ---- res-graph, echo = FALSE, fig.width = 6, fig.height = 3, fig.cap = "Visualization of the causal graph corresponding to the university admissions example introduced in Section \\ref{introduction} with the variable \\texttt{test} chosen as a \\textit{resolving variable} and therefore highlighted in red.", out.width = "60%"----

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

## ---- semi-graph, echo = FALSE, fig.width = 6, fig.height = 3, fig.cap = "Visualization of the causal graphical model also shown in Figure \\ref{fig:semi-markov}, obtained when passing a confounding matrix indicating a bidirectional edge between vertices \\texttt{test} and \\texttt{score} to \\texttt{fairadapt()}. The resulting Semi-Markovian setting is also handled by \\texttt{fairadapt()}, extending the basic Markovian formulation introduced in Section \\ref{markovian-scm-formulation}.", out.width = "60%"----

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

## ----session-info, include = FALSE----------------------------------
sessionInfo()

