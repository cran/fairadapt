#' Quantile engine constructor for the quantile learning step
#'
#' There are several methods that can be used for the quantile learning step
#' in the `fairadapt` package. Each of the methods needs a specific
#' constructor. The constructor is a function that takes the data (with some
#' additional meta-information) and returns an object on which the
#' `computeQuants()` generic can be called.
#'
#' Within the package, there are 3 different methods implemented, which use
#' quantile regressors based on linear models, random forests and neural
#' networks. However, there is additional flexibility and the user can provide
#' her/his own quantile method. For this, the user needs to write (i) the
#' constructor which returns an S3 classed object (see examples below);
#' (ii) a method for the `computeQuants()` generic for the S3 class
#' returned in (i).
#'
#' @details The `rangerQuants()` function uses random forests
#' (`ranger` package) for quantile regression.
#'
#' @param data A `data.frame` with data to be used for quantile
#' regression.
#' @param A.root A `logical(1L)` indicating whether the protected
#' attribute `A` is a root node of the causal graph. Used for splitting the
#' quantile regression.
#' @param ind A `logical` vector of length `nrow(data)`, indicating
#' which
#' samples have the baseline value of the protected attribute.
#' @param min.node.size Forwarded to [ranger::ranger()].
#' @param ... Forwarded to further methods.
#'
#' @return A `ranger` or a `rangersplit` S3 object, depending on the
#' value of the `A.root` argument, for `rangerQuants()`.
#'
#' @export
rangerQuants <- function(data, A.root, ind, min.node.size = 20, ...) {

  if (A.root) {
    return(
      structure(
        list(
          class0 = rangerQuants(data[ind, ], FALSE, NULL,
                                min.node.size = min.node.size, ...),
          class1 = rangerQuants(data[!ind, ], FALSE, NULL,
                                min.node.size = min.node.size, ...)),
        class = "rangersplit"
      )
    )
  }

  ranger::ranger(formula(data), data = data, quantreg = TRUE,
                 keep.inbag = TRUE, min.node.size = min.node.size, ...)
}

#' @details The `linearQuants()` function uses linear quantile regression
#' (`quantreg` package) for the Quantile Learning step.
#'
#' @param tau Forwarded to [quantreg::rq()] or [qrnn::mcqrnn.fit()].
#'
#' @return A `rqs` or a `quantregsplit` S3 object, depending on the
#' value of the `A.root` argument, for `linearQuants()`.
#'
#' @rdname rangerQuants
#' @export
linearQuants <- function(data, A.root, ind,
                         tau = c(0.001, seq(0.005, 0.995, by = 0.01), 0.999),
                         ...) {

  if (A.root) {
    return(
      structure(
        list(
          class0 = linearQuants(data[ind, ], FALSE, NULL, tau = tau, ...),
          class1 = linearQuants(data[!ind, ], FALSE, NULL, tau = tau, ...)
        ),
        class = "quantregsplit"
      )
    )
  }

  offending.cols <- 1L + which(
    vapply(seq_col(data)[-1L], function(x) length(unique(data[, x])), 1L) == 1
  )

  keep.cols <- which(!(seq_col(data) %in% offending.cols))

  if (length(offending.cols) == (ncol(data) - 1L)) {
    form <- as.formula(paste(names(data)[1L], "~ 1"))
  } else {
    form <- formula(data[, keep.cols])
  }

  quantreg::rq(form, data = data, tau = tau, ...)
}

#' @details The `mcqrnnQuants()` function uses  monotone quantile
#' regression neural networks (`mcqrnn` package) in the Quantile Learning step.
#'
#' @param iter.max Forwarded to [qrnn::mcqrnn.fit()].
#'
#' @return An ` mcqrnn` S3 object for ` mcqrnnQuants()`.
#'
#' @rdname rangerQuants
#' @export
mcqrnnQuants <- function(data, A.root, ind, tau = seq(0.005, 0.995, by = 0.01),
                         iter.max = 500, ...) {

  data.matrix <- matrix(as.numeric(unlist(data)), nrow = nrow(data))

  object <- qrnn::mcqrnn.fit(x = data.matrix[, -1L, drop = FALSE],
                             y = matrix(data.matrix[, 1L], ncol = 1),
                             tau = tau, n.trials = 1, iter.max = iter.max,
                             trace = FALSE, ...)

  structure(object, class = "mcqrnnobj")
}

#' Compute quantiles generic for the quantile learning step
#'
#' @param x Object with an associated `computeQuants()` method, to be used for
#' inferring quantiles.
#' @param data `data.frame` containing samples used in the quantile
#' regression.
#' @param newdata `data.frame` containing counterfactual values for which
#' the quantiles need to be inferred.
#' @param ind A `logical` vector of length `nrow(data)`, indicating which
#' samples have the baseline value of the protected attribute.
#' @param ... Additional arguments to be passed down to respective method
#' functions.
#'
#' @return A vector of counterfactual values corresponding to `newdata`.
#'
#' @export
computeQuants <- function(x, data, newdata, ind, ...) {
  UseMethod("computeQuants", x)
}

#' @export
computeQuants.ranger <- function(x, data, newdata, ind, test = FALSE,
                                 emp.only = FALSE, ...) {

  # GetQuants
  if (isTRUE(test)) {

    empirical <- predict(x, data = data[, -1L, drop = FALSE],
                         type = "quantiles", what = identity)
    empirical <- empirical$predictions
    if (emp.only) return(empirical)

  } else {

    empirical <- x$random.node.values.oob
  }

  quantiles <- predict(x, data = newdata, type = "quantiles",
                       what = identity)
  quantiles <- quantiles$predictions

  inferQuant(data, empirical, quantiles, ind)
}

#' @export
computeQuants.rangersplit <- function(x, data, newdata, ind, test = FALSE,
                                      emp.only = FALSE, ...) {

  # GetQuants
  if (isTRUE(test)) {

    empirical <- predict(x$class1, data = data[!ind, -1L, drop = FALSE],
                         type = "quantiles", what = identity)
    empirical <- empirical$predictions

  } else {

    empirical <- x$class1$random.node.values.oob
  }

  quantiles <- predict(x$class0, data = newdata, type = "quantiles",
                       what = identity)
  quantiles <- quantiles$predictions

  inferQuantsplit(data, empirical, quantiles, ind)
}

#' @export
computeQuants.rqs <- function(x, data, newdata, ind, emp.only = FALSE, ...) {

  empirical <- predict(x, newdata = data[, -1L, drop = FALSE])
  if (emp.only) return(empirical)
  quantiles <- predict(x, newdata = newdata)

  inferQuant(data, empirical, quantiles, ind)
}

#' @export
computeQuants.quantregsplit <- function(x, data, newdata, ind, ...) {

  empirical <- predict(x$class1, newdata = data[!ind, -1L, drop = FALSE])
  quantiles <- predict(x$class0, newdata = newdata)

  inferQuantsplit(data, empirical, quantiles, ind)
}

#' @export
computeQuants.mcqrnnobj <- function(x, data, newdata, ind, emp.only = FALSE,
                                    ...) {

  xmat <- matrix(as.numeric(unlist(data[, -1L, drop = FALSE])),
                 nrow = nrow(data))
  empirical <- qrnn::mcqrnn.predict(x = xmat, parms = x)
  if (emp.only) return(empirical)

  newx <- matrix(as.numeric(unlist(newdata)), ncol = ncol(newdata))
  quantiles <- qrnn::mcqrnn.predict(x = newx, parms = x)

  inferQuant(data, empirical, quantiles, ind)
}

inferQuant <- function(data, empirical, quantiles, ind) {

  eval <- data[, 1L]
  U.hat <- vapply(seq_row(data), function(x) ecdf(empirical[x, ]) (eval[x]),
                  numeric(1L))

  newU <- U.hat[!ind]

  vapply(seq_row(quantiles), function(x) quantile(quantiles[x, ], newU[x]),
         numeric(1L))
}

inferQuantsplit <- function(data, empirical, quantiles, ind) {

  eval <- data[!ind, 1L]
  U.hat <- vapply(seq_row(data[!ind, ]),
                  function(x) ecdf(empirical[x, ]) (eval[x]),
                  numeric(1L))

  newU <- U.hat

  vapply(seq_row(quantiles), function(x) quantile(quantiles[x, ], newU[x]),
         numeric(1L))
}
