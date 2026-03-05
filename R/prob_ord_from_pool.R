#' verdict probabilities based on jury pool sentiment for ordered verdict options.
#'
#' @param jury_n Integer. Number of jurors.
#' @param verdict_options Character vector of ordered verdict labels
#'   (e.g., `c("NG","M2","M1")`). Order matters: left = most lenient.
#'   Labels must be non-missing, non-empty, and unique.
#' @param verdict_props Numeric vector specifying proportion of jurors who support
#'   the respective verdict_options in the population from which jurors are drawn.
#'   (e.g., `c(.25,.50,.25)`). Must correspond to verdict_options, contain
#'   non-missing finite values, be non-negative, and sum to a positive value.
#' @param digits Integer. Optional, number of digits to round in the returned matrix.
#'
#' @return A vector of length K that are probabilities of the ordered verdicts.
#'
#' @seealso [transition.matrix.ordered]
#'
#' @examples
#' library(sate)
#'
#' # Three-verdict ordered model with a 12-person jury:
#' prob_ord_from_pool(12, c("NG", "M2", "M1"), c(.25,.50,.25), digits=3)
#'
#' @export
prob_ord_from_pool <- function(jury_n, verdict_options, verdict_props, digits = NULL) {
  assert_required(jury_n, verdict_options, verdict_props)
  assert_positive_integer(jury_n)
  if (!is.null(digits)) assert_nonnegative_integer(digits)
  if (!is.character(verdict_options)) {
    stop("verdict_options must be a character vector.", call. = FALSE)
  }
  if (length(verdict_options) < 2L) {
    stop("verdict_options must have at least two entries.", call. = FALSE)
  }
  if (any(is.na(verdict_options))) {
    stop("verdict_options cannot contain missing values.", call. = FALSE)
  }
  if (any(trimws(verdict_options) == "")) {
    stop("verdict_options cannot contain empty strings.", call. = FALSE)
  }
  if (anyDuplicated(verdict_options) > 0L) {
    stop("verdict_options must contain unique labels.", call. = FALSE)
  }

  if (!is.numeric(verdict_props)) {
    stop("verdict_props must be a numeric vector.", call. = FALSE)
  }
  if (any(is.na(verdict_props))) {
    stop("verdict_props cannot contain missing values.", call. = FALSE)
  }
  if (any(!is.finite(verdict_props))) {
    stop("verdict_props must contain only finite values.", call. = FALSE)
  }
  if (any(verdict_props < 0)) {
    stop("verdict_props must be non-negative.", call. = FALSE)
  }

  labs <- verdict_options
  K <- length(labs)
  if (jury_n < 1L) stop("jury_n must be >= 1.", call. = FALSE)

  # 1) Align & normalize population proportions
  if (!is.null(names(verdict_props))) verdict_props <- verdict_props[labs]
  if (length(verdict_props) != K) {
    stop("verdict_props must have the same length as verdict_options.", call. = FALSE)
  }
  if (any(is.na(verdict_props))) {
    stop("Named verdict_props must include all verdict_options labels.", call. = FALSE)
  }
  if (sum(verdict_props) <= 0) {
    stop("verdict_props must sum to a positive value.", call. = FALSE)
  }
  verdict_props <- verdict_props / sum(verdict_props)

  # 2) Build P once (so B uses the same state ordering/meta)
  P <- transition.matrix.ordered(jury_n, verdict_options, digits=99)
  meta <- attr(P, "meta")
  S <- meta$S
  states_mat <- meta$states_mat

  # 3) Starting-state distribution: Multinomial(n, verdict_props) (log-domain; zero-safe)
  logprob <- lgamma(jury_n + 1) - rowSums(lgamma(states_mat + 1))
  pos <- verdict_props > 0
  if (any(pos)) {
    logprob <- logprob + as.numeric(states_mat[, pos, drop = FALSE] %*% log(verdict_props[pos]))
  }
  if (any(!pos)) {
    bad <- rowSums(states_mat[, !pos, drop = FALSE]) > 0
    logprob[bad] <- -Inf
  }
  if (all(!is.finite(logprob))) stop("All starting states have zero probability with the given verdict_props.")
  m <- max(logprob[is.finite(logprob)])
  prob_of_state <- exp(logprob - m); prob_of_state[!is.finite(logprob)] <- 0
  prob_of_state <- prob_of_state / sum(prob_of_state)

  # 4) Absorption matrix (K x S): get from prob.ordered.verdicts()
  #    Use digits=NULL to avoid rounding zeros into existence/nonexistence.
  Bfull <- prob.ordered.verdicts(jury_n, labs,
                                 digits = NULL,        # no rounding inside B
                                 collab = TRUE)

  if (!all(dim(Bfull) == c(K, S))) stop("B matrix must be K x S to match state ordering.")

  # 5) Mix over starting-state distribution
  out <- as.numeric(Bfull %*% prob_of_state)
  out[out < 0] <- 0
  out <- out / sum(out)
  names(out) <- labs

  if (!is.null(digits) && length(digits) == 1 && is.finite(digits)) out <- round(out, digits)
  out
}
