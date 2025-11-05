#' Build column-stochastic transition matrix for ordered verdict options
#'
#' @description
#' Constructs the full **column-stochastic** Markov transition matrix \(P\) for a jury
#' deliberation model with an **ordered** set of verdict options (least → most punitive).
#' Transient states are all compositions of `jury_n` jurors across the `verdict_options`;
#' absorbing states are the `K` unanimity vertices (one per verdict), appended at the end
#' in the same order as `verdict_options`.
#'
#' The transition from a transient state is built by applying your **2-option step rule**
#' independently at each adjacent *cut* between options and combining those suggestions
#' with **equal weight across cuts**. For cut `r` (between options `r` and `r+1`), let
#' `g = sum(counts[(r+1):K])` be the number on the **more punitive** side; compute
#' \deqn{p_\text{up} = \left(0.5\frac{g-1}{n} + 0.25\right)^2,\quad
#'       p_\text{down} = \left(1 - 0.5\frac{g-1}{n} - 0.25\right)^2,\quad
#'       p_\text{stay} = 1 - p_\text{up} - p_\text{down}.}
#' Map “up” to moving one juror across the cut toward the more punitive option, “down”
#' toward the less punitive option; pool all “stay” mass (and any illegal move mass at
#' boundaries) into the **self-loop** so each column still sums to 1.
#'
#' @param jury_n Integer. Size of the jury (number of jurors), `jury_n >= 1`.
#' @param verdict_options Character vector of **ordered** verdict labels from least to most
#'   punitive, e.g. `c("NG","M2","M1")` or `c("NG","M3","M2","M1")`. The order defines
#'   which options are adjacent.
#' @param digits Optional integer. If supplied, round the returned matrix to this many
#'   decimals and then re-normalize each column to remain column-stochastic. Defaults to
#'   `NULL` (no rounding).
#'
#' @return A column-stochastic matrix `P` of size \eqn{S \times S}, where
#'   \eqn{S = \binom{n+K-1}{K-1}} is the number of compositions of `jury_n` into `K`
#'   parts (all states), ordered with **transients first** and then the `K` unanimity
#'   absorbing states in the order of `verdict_options`. The matrix carries metadata on
#'   `attr(P, "meta")` as a list with elements:
#'   \itemize{
#'     \item `states` — list of length-`K` integer count vectors for each state, in column order;
#'     \item `idx` — environment mapping comma-joined count vectors to column/row indices;
#'     \item `T`, `S`, `K`, `n` — counts (transients, total states, #options, #jurors);
#'     \item `verdict_options` — the label vector you supplied.
#'   }
#'
#' @details
#' * Absorbing columns (the last `K`) are identity columns (unanimity stays put).
#' * Self-loops collect “stay” mass from all cuts and any mass from moves that are illegal
#'   at boundaries (e.g., trying to move from an empty option).
#' * Providing `digits` is meant for tidy printing; for numerical work you may prefer to
#'   leave `digits = NULL` to keep full precision.
#'
#' @examples
#' # 3 jurors, 3 options (NG < M2 < M1), equal cut weights
#' P <- transition.matrix.ordered(3, c("NG","M2","M1"))
#' dim(P); colSums(P)                        # columns sum to 1
#' attr(P, "meta")$verdict_options           # labels carried in metadata
#'
#' # Tidy print:
#' transition.matrix.ordered(3, c("NG","M2","M1"), digits = 3)
#'
#' # 4 options (NG < M3 < M2 < M1)
#' P4 <- transition.matrix.ordered(3, c("NG","M3","M2","M1"))
#'
#' @seealso \code{\link{prob.ordered.verdicts}} for solved absorption probabilities
#'   (including appended unanimity starts) built on top of this transition matrix.
#' @importFrom stats setNames
#' @export
transition.matrix.ordered <- function(jury_n, verdict_options, digits = NULL) {
  n <- as.integer(jury_n)
  labs <- as.character(verdict_options)
  K <- length(labs)
  stopifnot(n >= 1, K >= 2)

  absorb_threshold = n  # hard coded * (8/12) to replicate a Hastie analysis

  # ---------- absorbing threshold ----------
  if (is.null(absorb_threshold)) {
    thr <- n
  } else {
    thr <- as.integer(absorb_threshold)
    stopifnot(thr >= 1L, thr <= n)
  }

  # ---- enumerate all K-part compositions of n; transients first, then unanimities (labs order)
  enumerate_counts <- function(n, K) {
    out <- list()
    rec <- function(pos, rem, pref) {
      if (pos == K) out[[length(out)+1]] <<- c(pref, rem)
      else for (v in 0:rem) rec(pos+1, rem - v, c(pref, v))
    }
    rec(1, n, c()); out
  }
  all <- enumerate_counts(n, K)

  is_abs <- function(v) any(v >= thr)

  # unanimity: put the K unanimities last, in verdict_options order
  absorbers <- lapply(seq_len(K), function(k){ v <- integer(K); v[k] <- n; v })
  trans     <- Filter(function(v) sum(v == n) == 0, all)
  states    <- c(trans, absorbers)
  Tn         <- length(trans)
  A          <- length(absorbers)
  S          <- Tn + A
  states_mat <- do.call(rbind, states)

  # fast index map
  keys  <- vapply(states, function(v) paste(v, collapse=","), character(1))
  key2i <- as.environment(list2env(stats::setNames(as.list(seq_along(keys)), keys)))
  get_idx <- function(v) get(paste(v, collapse=","), envir=key2i, inherits=FALSE)

  # ---- 2-option triad from your earlier model (uses "more punitive" count g)
  two_option_triad <- function(g, n, bias = 0) {
    margin_more <- 0.5 * ((g - bias) / n) + 0.25
    margin_more <- max(0, min(1, margin_more))                        # clamp for safety
    p_add  <- margin_more^2
    p_lose <- (1 - margin_more)^2
    p_same <- 1 - p_add - p_lose
    tri <- c(up=p_add, down=p_lose, stay=p_same)
    tri / sum(tri)
  }

  # ---- move one juror across cut r (1..K-1)
  move_across <- function(x, r, dir=c("up","down")) {
    dir <- match.arg(dir); y <- x
    if (dir == "up")   { if (y[r]   <= 0) return(NULL); y[r]   <- y[r]-1; y[r+1] <- y[r+1]+1 }
    if (dir == "down") { if (y[r+1] <= 0) return(NULL); y[r+1] <- y[r+1]-1; y[r]   <- y[r]+1 }
    y
  }

  # ---- build P (rows=to, cols=from), column-stochastic
  P <- matrix(0, S, S)
  equal_w <- rep(1/(K-1), K-1)  # equal weight for each adjacent cut
  # presumption on NG cut only
  cut_bias <- c(1, rep(0, K-2))

  for (c in seq_len(Tn)) {
    x <- states[[c]]
    placed <- 0
    for (r in 1:(K-1)) {
      g_right <- sum(x[(r+1):K])          # "more punitive" count to the right of cut r
      tri <- two_option_triad(g_right, n, bias = cut_bias[r]) # c(up, down, stay)

      # up across cut r (r -> r+1)
      y_up <- move_across(x, r, "up")
      if (!is.null(y_up)) {
        P[get_idx(y_up), c] <- P[get_idx(y_up), c] + equal_w[r] * tri["up"]
        placed <- placed + equal_w[r] * tri["up"]
      }

      # down across cut r (r+1 -> r)
      y_dn <- move_across(x, r, "down")
      if (!is.null(y_dn)) {
        P[get_idx(y_dn), c] <- P[get_idx(y_dn), c] + equal_w[r] * tri["down"]
        placed <- placed + equal_w[r] * tri["down"]
      }
      # stay mass across cuts is pooled into the self-loop below
    }
    # self-loop gets all remaining mass (stay + illegal moves)
    P[c, c] <- P[c, c] + (1 - placed)
  }

  # absorbing columns: identity for EVERY absorbing state (threshold reached)
  if (A > 0) {
    for (j in seq_len(A)) {
      col <- Tn + j
      P[Tn + j, col] <- 1
    }
  }

  # normalize defensively
  P <- sweep(P, 2, pmax(colSums(P), 1e-15), "/")

  # optional rounding for tidy display (then re-normalize)
  if (!is.null(digits)) {
    P <- round(P, digits)
    P <- sweep(P, 2, pmax(colSums(P), 1e-15), "/")
  }

  # metadata
  attr(P, "meta") <- list(
    states       = states,
    states_mat   = states_mat,
    idx          = key2i,
    T            = Tn,           # # transient states
    A            = A,            # # absorbing states (can be > K)
    S            = S, K = K, n = n,
    verdict_options = labs,
    absorb_threshold = thr
  )

  P
}
