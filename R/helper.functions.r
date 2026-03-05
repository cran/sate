
## Internal input validation helpers -----------------------------------------
#' Assert numeric values are between 0 and 1 (inclusive)
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_between_0_1 <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))

  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_between_0_1().", call. = FALSE)
  }

  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]

    if (!is.numeric(x) || any(is.na(x)) || any(!is.finite(x))) {
      stop(nm, " must be numeric with finite, non-missing value(s).", call. = FALSE)
    }
    if (any(x < 0) || any(x > 1)) {
      stop(nm, " must be between 0 and 1 (inclusive).", call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Assert numeric values are strictly positive
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_positive_numeric <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))

  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_positive_numeric().", call. = FALSE)
  }

  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]

    if (!is.numeric(x) || any(is.na(x)) || any(!is.finite(x)) || any(x <= 0)) {
      stop(nm, " must be numeric with finite, non-missing value(s) > 0.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Assert scalar positive integers
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_positive_integer <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))

  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_positive_integer().", call. = FALSE)
  }

  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]

    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0 || x %% 1 != 0) {
      stop(nm, " must be a single positive integer.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Assert scalar non-negative integers
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_nonnegative_integer <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))

  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_nonnegative_integer().", call. = FALSE)
  }

  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]

    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x %% 1 != 0) {
      stop(nm, " must be a single non-negative integer.", call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' Assert required arguments are supplied
#'
#' @param ... Arguments to check.
#' @keywords internal
#' @noRd
assert_required <- function(...) {
  miss <- missing_args(...)
  if (length(miss)) {
    stop(
      "Missing required argument(s): ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}



#' Evaluate code with an optional local random seed
#'
#' @param seed Optional non-negative integer seed. If NULL, current RNG state is used.
#' @param fn Function with no arguments to evaluate under local seed control.
#' @keywords internal
#' @noRd
with_local_seed <- function(seed, fn) {
  if (!is.function(fn)) {
    stop("fn must be a function.", call. = FALSE)
  }
  if (is.null(seed)) {
    return(fn())
  }

  assert_nonnegative_integer(seed)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  base::set.seed(seed)
  fn()
}

#' Extract the jury guilty-probability value from stats output
#'
#' @param x One-row data.frame or named vector containing probability column.
#' @param label Object label used in error messages.
#' @keywords internal
#' @noRd
get_pg_value <- function(x, label = "x") {
  if ("P(G)" %in% names(x)) return(x[["P(G)"]])
  if ("PG" %in% names(x)) return(x[["PG"]])
  stop(label, " must contain a probability column named 'P(G)' or 'PG'.", call. = FALSE)
}
#' Helper function to calculate the 90 percent confidence interval of a proportion.
#'
#' @param m The sample proportion (e.g. proportion of jurors who favor a guilty verdict).
#' @param se The standard error of the sample proportion, m.
#' @return Returns the 90 percent confidence interval as a list.
#' @description Calculates the 90 percent confidence interval of a proportion. 90 percent confidence interval used to test one-sided hypothesis at .05 level.
#' @examples
#'    library(sate)
#'    CI90(m=.5, se=.04)
#'
#'    CI90(m=10/12, se=.02)
#' @keywords internal
#' @importFrom stats qnorm
#' @noRd
CI90 <- function(m, se)
{
  assert_required(m, se)
  if(!base::is.numeric(m)) stop("m must be a number.")
  if(!base::is.numeric(se) || (se < 0)) stop("se must be non-negative number.")

  lower = m - stats::qnorm(0.95) * se
  upper = m + stats::qnorm(0.95) * se
  out1 = base::c(lower, upper)
  # print(out1)
  base::names(out1) = base::c("Lower 90% CI", "     Upper 90% CI")
  return(out1)
}



#' Creates the shell of a plot used to display estimate of harm relative to harm threshold
#'
#' @return No return
#' @description Creates the shell of a plot used for compact display estimate of harm estimate
#'              relative to harm thresholds.
#' @examples
#'    library(sate)
#'    compact_harm_plot()
#' @export
#' @importFrom graphics axis mtext par plot rect
compact_harm_plot <- function()
{

  revert.par <- graphics::par(c("mar", "family", "mfrow", "mgp"))
  graphics::par(mar=base::c(2.5, 1, 1, 1), family="serif", mfrow=base::c(1,1), mgp=base::c(0,.5,.5))
  Tvalue <- .10
  graphics::plot(x="", y="", xlab="", ylab="",
       xlim=base::c(-.1, .425), ylim=base::c(0, .1), axes=F)
  graphics::axis(side=1, at=base::c(-.5,-.1,0,.1,.2,.3,.4,.5,.6),
       labels=base::c("","-.1",".0", ".1", ".2", ".3",".4",".5",""),
       line=.1, cex.axis=.7, gap.axis=0)

  graphics::rect(ybottom=-1, ytop=1, xleft=-1, xright=0, col = "white", border = F)
  graphics::rect(ybottom=-1, ytop=1, xleft=0, xright=Tvalue, col = "gray90", border = F)
  graphics::rect(ybottom=-1, ytop=1, xleft=Tvalue, xright=1, col = "gray80", border = F)
  graphics::mtext(text=base::c("No Harm", "Tolerable Harm", "Intolerable Harm"),
                  at=base::c(-.08, .05, .25), side=3, cex=.75, line=.20)
  graphics::par(revert.par)

}


#' Helper function to retrieve P(G|K) values. This should be deprecated by get_pG_by_k.
#'
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns a vector of guilty verdict probabilities for K values 0 to jury_n.
#' @description Returns a vector of P(G|K) values based on jury size. Probabilities
#'              can be generated for any jury_n (greater than 1).
#' @examples
#'    library(sate)
#'    get.model.values(12)
#'
#'    get.model.values(jury_n=8)
#' @keywords internal
#' @noRd
get.model.values <- function(jury_n)
{
  assert_required(jury_n)
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("jury_n must be positive integer.")
  # jury_n = 12
  P_matrix <- transition.matrix(jury_n = jury_n)
  A_matrix <- P_matrix[, c(1, jury_n+1)]
  R_matrix <- P_matrix[c(1, jury_n+1), -c(1, jury_n+1)]
  Q_matrix <- P_matrix[-c(1, jury_n+1), -c(1, jury_n+1)]
  I_matrix <- diag(jury_n - 1)
  B_matrix <- R_matrix %*% solve(I_matrix - Q_matrix)
  pG_by_k <- c(0, B_matrix[2,], 1)
  return(pG_by_k)
}


missing_args <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  labels <- vapply(exprs, deparse1, character(1))
  target_env <- parent.frame(2L)

  is_missing <- vapply(exprs, FUN = function(sym) {
    if (!is.symbol(sym)) return(FALSE)
    eval(bquote(missing(.(sym))), envir = target_env)
  }, logical(1))

  labels[is_missing]
}

#' Helper function use to plot point estimates with confidence intervals
#'
#' @param pg The proportion of jurors who favor a guilty verdict
#' @param n The size of the sample used to estimate pg
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param point.col The color of point at center of ellipse; default is "gray25"
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @param simulate Simulation control passed to `as.jury.stats`. Use `FALSE`
#'   (default), `TRUE`, or a named list like `list(nDraws=5000, seed=12345)`.
#' @return No return (adds element to plot)
#' @description Plots probability of guilty verdict with confidence interval based on juror-level
#'              statistics. This is a helper function, called by graphing functions to add layer to open graph.
#' @examples
#'    library(sate)
#'    plot(x="", y="", xlim=c(0,1), ylim=c(0, 1))
#'    plot.ellipse(pg=.70, n=400)
#'
#'    plot.ellipse(pg=.75, n=450, jury_n=6, pstrikes=3, dstrikes=3)
#' @keywords internal
#' @importFrom ellipse ellipse
#' @importFrom graphics lines points polygon
#' @noRd
plot.ellipse <- function(pg, n, jury_n=12, point.col="gray25", pstrikes=0, dstrikes=0, accuracy=.15, simulate=FALSE)
{
  assert_required(pg, n)
  assert_between_0_1(pg)
  assert_positive_numeric(n)

  # general form
  ci_color <- "#BCBCBC70"
  jurors.se  <- se.prop(p=pg, n=n)
  jury.stats <- as.jury.stats(sample_pg=pg, sample_n=n, jury_n=jury_n, pstrikes=pstrikes,
                              dstrikes=dstrikes, accuracy=accuracy, digits=99, simulate=simulate)
  # the +/- .01 here is used for slope of line at point
  A <- (as.jury.point(sample_pg=pg +.01, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes,
                      accuracy=accuracy, simulate=simulate) -
          as.jury.point(sample_pg=pg -.01, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes,
                        accuracy=accuracy, simulate=simulate)) / .02
  H <- base::sqrt(1 + A^2)
  rotation_matrix <- base::matrix(nrow=2, byrow=T, data=base::c(A/H, -1/H, 1/H, A/H))
  ellipse <- ellipse::ellipse(x=0, centre=c(0, 0),
                              scale=c(jurors.se, jury.stats$SE),
                              level=.90) %*% rotation_matrix
  pG_value <- get_pg_value(jury.stats, "jury.stats")
  ellipse[,1] <- ellipse[,1] + pg
  ellipse[,2] <- ellipse[,2] + pG_value
  graphics::polygon(ellipse[,1], ellipse[,2], col=ci_color, border=F)
  graphics::lines(ellipse, col="gray25", lty=3)
  graphics::points(x=pg, y=pG_value, col=point.col, pch=16)
  graphics::points(x=pg, y=pG_value, col="black", pch=1)
}



#' Helper function to obtain expected rounds of deliberation to verdict.
#'
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns a vector of expected number of rounds of deliberation to reach a verdict for K values 0 to jury_n.
#' @description Retrieves vector of P(G|K) values based on jury size. These probabilities
#'              can be generated but accessing stored values speeds computations.
#' @examples
#'    library(sate)
#'    rounds_to_verdict(12)
#'
#'    rounds_to_verdict(jury_n=8)
#' @keywords internal
#' @noRd
rounds_to_verdict <- function(jury_n)
{
  assert_required(jury_n)
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("jury_n must be positive integer.")

  # rounds to verdict
  P_matrix <- transition.matrix(jury_n = jury_n)
  A_matrix <- P_matrix[, c(1, jury_n+1)]
  R_matrix <- P_matrix[c(1, jury_n+1), -c(1, jury_n+1)]
  Q_matrix <- P_matrix[-c(1, jury_n+1), -c(1, jury_n+1)]
  I_matrix <- diag(jury_n - 1)
  B_matrix <- R_matrix %*% solve(I_matrix - Q_matrix)
  ones <- rep(1, jury_n - 1)
  T_transient <- solve(I_matrix - t(Q_matrix), ones)
  T_all <- c(1, as.numeric(T_transient), 1)
  return(T_all)
}



#' Helper function to Calculate the standard error of proportion.
#'
#' @param p The proportion (e.g. proportion of jurors who favor a guilty verdict).
#' @param n The size of the sample used to estimate p.
#' @return Returns the standard error of a sample proportion.
#' @description Calculates the standard error of proportion.
#' @examples
#'    library(sate)
#'    se.prop(p=.50, n=500)
#'
#'    se.prop(p=10/12, n=400)
#' @keywords internal
#' @noRd
se.prop <- function(p, n)
{
  assert_required(p, n)
  assert_between_0_1(p)
  assert_positive_numeric(n)

  se = base::sqrt(p*(1-p)/n)
  # print(se)
  return(se)
}


