
#' @keywords internal
#' @import stats
#' @importFrom assertthat assert_that
#' @aliases fairadapt-package
"_PACKAGE"

#' Fair data adaptation (fairadapt)
#'
#' Implementation of fair data adaptation with quantile preservation
#' (Plecko & Meinshausen, 2020). Uses only plain `R`.
#'
#' The procedure takes the training and testing data as an input, together with
#' the causal graph given by an adjacency matrix and the list of resolving
#' variables, which should be kept fixed during the adaptation procedure. The
#' procedure then calculates a fair representation of the data, after which
#' any classification method can be used. There are, however, several valid
#' training options yielding fair predictions, and the best of them can be
#' chosen with cross-validation. For more details we refer the user to the
#' original paper. Most of the running time is due to the quantile regression
#' step using the ranger package.
#'
#' @param formula Object of class `formula` describing the response and
#' the covariates.
#' @param prot.attr A value of class `character` describing the binary
#' protected attribute. Must be one of the entries of `colnames(adj.mat)`.
#' @param adj.mat Matrix of class `matrix` encoding the relationships in
#' the causal graph. `M[i,j] == 1L` implies the existence of an edge from
#' node i to node j. Must include all the variables appearing in the formula
#' object. When the `adj.mat` argument is set to `NULL`, then the
#' `top.ord` argument has to be supplied.
#' @param train.data,test.data Training data & testing data, both of class
#' `data.frame`. Test data is by default `NULL`.
#' @param cfd.mat Symmetric matrix of class `matrix` encoding the
#' bidirected edges in the causal graph. `M[i,j] == M[j, i] == 1L`
#' implies the existence of a bidirected edge between nodes i and j. Must
#' include all the variables appearing in the formula object.
#' @param top.ord A vector of class `character` describing the
#' topological ordering of the causal graph. Default value is `NULL`,
#' but this argument must be supplied if `adj.mat` is not specified.
#' Also must include all the variables appearing in the formula object.
#' @param res.vars A vector of class `character` listing all the resolving
#' variables, which should not be changed by the adaption procedure. Default
#' value is `NULL`, corresponding to no resolving variables. Resolving
#' variables should be a subset of the descendants of the protected attribute.
#' @param quant.method A function choosing the method used for quantile
#' regression. Default value is `rangerQuants` (using random forest
#' quantile regression). Other implemented options are `linearQuants` and
#' `mcqrnnQuants`. A custom function can be supplied by the user here,
#' and the associated method for the S3 generic `computeQuants` needs to be
#' added.
#' @param visualize.graph A `logical` indicating whether the causal graph
#' should be plotted upon calling the `fairadapt()` function. Default
#' value is `FALSE`.
#' @param eval.qfit Argument indicating whether the quality of the quantile
#' regression fit should be computed using cross-validation. Default value is
#' `NULL`, but whenever a positive integer value is specified, then it is
#' interpreted as the number of folds used in the cross-validation procedure.
#' @param ... Additional arguments forwarded to the function passed as
#' `quant.method`.
#'
#' @return An object of class `fairadapt`, containing the original and
#' adapted training and testing data, together with the causal graph and some
#' additional meta-information.
#' @examples
#' n_samp <- 200
#' uni_dim <- c(       "gender", "edu", "test", "score")
#' uni_adj <- matrix(c(       0,     1,      1,       0,
#'                            0,     0,      1,       1,
#'                            0,     0,      0,       1,
#'                            0,     0,      0,       0),
#'                   ncol = length(uni_dim),
#'                   dimnames = rep(list(uni_dim), 2),
#'                   byrow = TRUE)
#'
#' uni_ada <- fairadapt(score ~ .,
#'   train.data = head(uni_admission, n = n_samp),
#'   test.data = tail(uni_admission, n = n_samp),
#'   adj.mat = uni_adj,
#'   prot.attr = "gender"
#' )
#'
#' uni_ada
#'
#' @references
#' Plecko, D. & Meinshausen, N. (2020).
#' Fair Data Adaptation with Quantile Preservation.
#' Journal of Machine Learning Research, 21(242), 1-44.
#'
#' Plecko, D. & Bennett, N. & Meinshausen, N. (2024).
#' fairadapt: Causal reasoning for fair data pre-processing.
#' Journal of Statistical Software, 110(4).
#' \doi{10.18637/jss.v110.i04}.
#'
#' @export
fairadapt <- function(formula, prot.attr, adj.mat, train.data, test.data = NULL,
  cfd.mat = NULL, top.ord = NULL, res.vars = NULL, quant.method = rangerQuants,
  visualize.graph = FALSE, eval.qfit = NULL, ...) {

  if (missing(adj.mat)) {
    adj.mat <- NULL
  }

  # verify correctness of input
  correctInput(formula, train.data, test.data, adj.mat, cfd.mat, top.ord,
               prot.attr, res.vars, quant.method)

  # reorder the adjacency matrix if necessary
  adj.mat <- adj.mat[colnames(adj.mat), ]

  # reorder columns and Factor-ize
  train.data <- model.frame(formula, train.data)
  train.data[, prot.attr] <- as.factor(train.data[, prot.attr])
  train.len <- nrow(train.data)

  if (!is.null(test.data)) {

    test.data <- test.data[, colnames(train.data)[-1L]]
    test.data[, prot.attr] <- as.factor(test.data[, prot.attr])
    test.data <- cbind(NA, test.data)
    colnames(test.data) <- colnames(train.data)
  }

  org.data <- rbind(train.data, test.data)
  full.len <- nrow(org.data)

  # keep important parts of adjacency matrix
  adj.mat <- adj.mat[colnames(org.data), colnames(org.data)]

  if (is.null(cfd.mat) && !is.null(adj.mat)) {

    cfd.mat <- adj.mat
    cfd.mat[, ] <- 0
  }

  # construct the initial version of adapted data
  adapt.data <- org.data
  base.lvl <- levels(org.data[, prot.attr])[1L]
  attr.lvls <- levels(org.data[, prot.attr])
  base.ind <- org.data[, prot.attr] == base.lvl
  adapt.data[, prot.attr] <- factor(base.lvl,
                                    levels = levels(org.data[, prot.attr]))

  # obtain topological ordering and descendants of A
  if (is.null(top.ord)) {

    # Markovian / Semi-Markovian case

    top.ord <- topologicalOrdering(adj.mat)
    attr.des <- getDescendants(prot.attr, adj.mat)
    attr.root <- (length(getParents(prot.attr, adj.mat)) == 0L) &&
      (length(confoundedComponent(prot.attr, cfd.mat)) == 1L)

    ig <- graphModel(adj.mat, cfd.mat)

    if (visualize.graph) {
      plot(ig)
    }

    # fail if nonId
    if (nonId(c(prot.attr, res.vars), adj.mat, cfd.mat)) {
      stop("The desired intervention is non-identifiable")
    }

  } else {

    # Topological Ordering case
    attr.des <- getDescendants(prot.attr, adj.mat, top.ord)
    attr.root <- top.ord[1L] == prot.attr
    ig <- NULL
  }

  q.engine <- list()

  # main procedure part
  var.ind <- seq.int(which(top.ord == prot.attr) + 1L, length(top.ord))

  for (curr.var in top.ord[var.ind]) {

    # check if this variable is skipped
    # must change for topological order approach/also for when A is not root

    changed.parents <- intersect(getParents(curr.var, adj.mat, top.ord),
                                 union(attr.des, prot.attr))

    if (sum(!is.element(changed.parents, res.vars)) == 0) {
      res.vars <- c(res.vars, curr.var)
    }

    if (is.element(curr.var, res.vars) | !is.element(curr.var, attr.des)) {
      next
    }

    q.engine[[curr.var]] <- list()

    type <- class(org.data[, curr.var])
    q.engine[[curr.var]][["type"]] <- type

    discrete <- FALSE
    q.engine[[curr.var]][["discrete"]] <- discrete

    curr.parents <- adjustmentSet(curr.var, adj.mat, cfd.mat, top.ord)
    q.engine[[curr.var]][["parents"]] <- curr.parents

    row.idx <- rep(TRUE, full.len)

    if (curr.var == colnames(train.data)[1L]) {
      row.idx[-seq_len(train.len)] <- FALSE
    }

    # check if Discrete
    if (length(unique(org.data[, curr.var])) < 10 |
        is.factor(org.data[, curr.var]) | is.integer(org.data[, curr.var]) |
        is.character(org.data[, curr.var])) {

      discrete <- TRUE
      q.engine[[curr.var]][["discrete"]] <- discrete

      if (is.character(org.data[, curr.var])) {
        org.data[, curr.var] <- factor(org.data[, curr.var])
      }

      if (is.factor(org.data[, curr.var])) {
        org.data[, curr.var] <- factor(
          org.data[, curr.var], levels = catOrder(org.data[row.idx, 1L],
                                                  org.data[row.idx, curr.var])
        )
      } else if (is.integer(org.data[, curr.var])) {
        q.engine[[curr.var]][["discrete"]] <- discrete <- 1L
      } else {
        org.data[, curr.var] <- factor(org.data[, curr.var])
      }

      unique.values <- levels(org.data[, curr.var])
      q.engine[[curr.var]][["unique.values"]] <- unique.values

      int.enc <- as.integer(org.data[row.idx, curr.var]) +
                 runif(length(org.data[row.idx, curr.var]), -0.5, 0.5)

      adapt.data[, curr.var] <- makeLength(int.enc, train.len, full.len)
      org.data[, curr.var] <- adapt.data[, curr.var]
    }

    # perform the Adaptation
    qr.data <- org.data[row.idx, c(curr.var, curr.parents), drop = FALSE]
    cf.parents <- adapt.data[!base.ind & row.idx, curr.parents, drop = FALSE]

    assert_that(ncol(qr.data) == (ncol(cf.parents) + 1L))

    object <- quant.method(qr.data, attr.root, base.ind[row.idx], ...)
    q.engine[[curr.var]][["object"]] <- object

    adapt.data[!base.ind & row.idx, curr.var] <-
      computeQuants(object, qr.data, cf.parents, base.ind[row.idx])

    if (!is.null(eval.qfit)) {
      eval.qfit <- as.integer(eval.qfit)
      assert_that(eval.qfit > 1L,
                  msg = "`eval.qfit` argument must be a positive integer.")
      # split into folds
      folds <- as.integer(cut(seq_row(qr.data), breaks = eval.qfit))
      q.fold <- c()
      for (fold in seq_len(eval.qfit)) {
        fold.ind <- folds == fold
        obj <- quant.method(qr.data[!fold.ind, ], A.root = FALSE, ind = NULL,
                            ...)

        q.fold <- rbind(
          q.fold, computeQuants(obj, qr.data[fold.ind, ], newdata = NULL,
                                ind = NULL, test = TRUE, emp.only = TRUE)
        )

      }

      q.engine[[curr.var]][["qfit.score"]] <- qfitScore(qr.data[[curr.var]],
                                                        q.fold)
    }

    # check if there exists a resolving ancestor
    ancestors <- getAncestors(curr.var, adj.mat, top.ord)
    res.anc <- (sum(is.element(ancestors, res.vars)) > 0)

    # enforce Marginal Matching if there is no resolving ancestor & discrete
    if (discrete & !res.anc & attr.root) {
      adapt.data[row.idx, curr.var] <-
        marginalMatching(adapt.data[row.idx, curr.var], base.ind[row.idx])
    }

    # if discrete, recode back to discrete or factor
    if (discrete) {

      if (is.integer(discrete)) {

        adapt.data[, curr.var] <- as.integer(round(adapt.data[, curr.var]))
        org.data[, curr.var] <- as.integer((round(org.data[, curr.var])))

      } else {
        adapt.data[, curr.var] <-
          decodeDiscrete(adapt.data[row.idx, curr.var], unique.values, type,
                         full.len)
        org.data[, curr.var] <-
          decodeDiscrete(org.data[row.idx, curr.var], unique.values, type,
                         full.len)
      }

    }

  }

  structure(
    list(
      adapt.train = adapt.data[seq_len(train.len), ],
      adapt.test = adapt.data[-seq_len(train.len), -1L],
      train = train.data,
      test = test.data,
      base.lvl = base.lvl,
      attr.lvls = attr.lvls,
      base.ind = base.ind,
      formula = formula,
      res.vars = res.vars,
      prot.attr = prot.attr,
      graph = ig,
      quant.method = deparse(substitute(quant.method)),
      adapt.call = match.call(),
      adj.mat = adj.mat,
      cfd.mat = cfd.mat,
      top.ord = top.ord,
      q.engine = q.engine
    ),
    class = "fairadapt"
  )
}
