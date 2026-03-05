#' Absorption probabilities for ordered-category jury models
#'
#' Compute the probability that an ordered-category Markov chain on jury vote
#' counts will eventually absorb at each unanimous verdict, starting from any
#' transient (non-unanimous) composition. Internally, this constructs the
#' transition matrix with `transition.matrix.ordered()` **using its defaults**,
#' i.e., equal cut lines (no lambda weighting).
#'
#' @param jury_n Integer. Number of jurors.
#' @param verdict_options Character vector of ordered verdict labels
#'   (e.g., `c("NG","Lesser","G")`). Order matters: left = most lenient.
#' @param digits Integer. Number of digits to round in the returned matrix.
#'   Default `3`.
#' @param collab Logical. If `TRUE` (default), attach human-friendly row/column
#'   labels: rows are verdict names; columns are starting states (transients
#'   first, then unanimities).
#'
#' @details
#' Let \eqn{P} be the transition matrix returned by
#' `transition.matrix.ordered(jury_n, verdict_options)`, with meta attributes
#' providing:
#' - `T`: number of transient states,
#' - `K`: number of absorbing states (equal to `length(verdict_options)`),
#' - `states`: list of length `T + K` of count vectors (per state),
#' - `n`: the jury size,
#' - `verdict_options`: the verdict labels.
#' @return A numeric K by T+K matrix of absorption probabilities.
#'   Rows index absorbing verdicts in `verdict_options`. Columns index starting
#'   states: first all transient compositions, then each unanimity composition
#'   (one per verdict). If `collab = TRUE`, row/column names are added.
#'
#' @seealso [transition.matrix.ordered]
#'
#' @examples
#' library(sate)
#'
#' # Three-verdict ordered model with a 12-person jury:
#' prob.ordered.verdicts(12, c("NG", "M2", "M1"))
#'
#' # Probability of ultimately unanimous "Lesser" starting from A=6, B=4, C=2:
#' prob.ordered.verdicts(12, c("A","B","C"), digits = 3)
#'
#' @export
prob.ordered.verdicts <- function(jury_n,
                                  verdict_options,
                                  digits = NULL,
                                  collab = TRUE) {
  assert_required(jury_n, verdict_options)
  assert_positive_integer(jury_n)
  if (!is.null(digits)) assert_nonnegative_integer(digits)
  if (!is.logical(collab) || length(collab) != 1L || is.na(collab)) {
    stop("collab must be TRUE or FALSE.", call. = FALSE)
  }

  P <- transition.matrix.ordered(jury_n, verdict_options, digits=99)
  meta <- attr(P, "meta")
  T <- meta$T; S <- meta$S; K <- meta$K
  states_mat <- meta$states_mat
  # number of absorbing states (may exceed K when threshold < n)
  A <- S - T
  if (A <= 0) stop("No absorbing states found (check threshold).")

  # Partition P (column-stochastic: rows=to, cols=from)
  Q <- as.matrix(P[1:T, 1:T, drop = FALSE])          # T x T
  R_abs <- as.matrix(P[(T+1):S, 1:T, drop = FALSE])  # A x T

  # Defensive checks
  if (!all(dim(Q) == c(T, T))) stop("Q must be T x T.")
  if (!all(dim(R_abs) == c(A, T))) stop("R must be A x T.")

  # Fundamental matrix and absorption to each absorbing state (A x T)
  Nfund <- solve(diag(T) - Q)
  B_abs <- R_abs %*% Nfund

  # Map each absorbing state to the verdict that triggered absorption
  thr <- meta$absorb_threshold
  if (is.null(thr)) thr <- meta$n  # unanimity fallback
  winners <- integer(A)
  for (a in 1:A) {
    v <- states_mat[T + a, ]               # counts at absorbing state a
    w <- which(v >= thr)
    if (length(w) != 1) {
      # In typical thresholds (>= ceil(n/2)+1), there will be exactly one
      # But guard anyway: pick the first deterministically
      w <- w[1]
    }
    winners[a] <- w
  }

  # Aggregate A absorbing states to K verdict categories
  M <- matrix(0, nrow = K, ncol = A)
  for (a in 1:A) M[winners[a], a] <- 1

  # K x T absorption for transient starts, then add absorbing-start columns
  Bk <- M %*% B_abs                       # K x T
  B_absorb_cols <- M                      # K x A (identity per winner)
  Bfull <- cbind(Bk, B_absorb_cols)

  # Optional rounding (display only), then re-normalize columns to 1
  if (!is.null(digits) && length(digits) == 1 && is.finite(digits)) {
    Bfull <- round(Bfull, digits)
    Bfull <- sweep(Bfull, 2, pmax(colSums(Bfull), 1e-15), "/")
  }

  rownames(Bfull) <- meta$verdict_options
  if (isTRUE(collab)) {
    # Build "(0, 1, 11)"-style labels from each numeric vector in the list
    state_labels <- vapply(
      meta$states,
      function(v) sprintf("(%s)", paste(formatC(v, digits = 0, format = "fg"), collapse = ", ")),
      FUN.VALUE = character(1)
    )

    # Assign only if the lengths match to avoid the dimnames error
    if (length(state_labels) == ncol(Bfull)) {
      colnames(Bfull) <- state_labels
    } else {
      warning(sprintf(
        "Skipping colnames(Bfull): length(state_labels)=%d != ncol(Bfull)=%d",
        length(state_labels), ncol(Bfull)
      ))
    }
  }
  Bfull
}

