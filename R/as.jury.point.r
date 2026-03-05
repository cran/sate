#' Calculates probability a jury will find defendant guilty based on juror preferences
#'
#' @param sample_pg Proportion of jurors who favor a guilty verdict. Can be a single number between 0 and 1, or a vector of such numbers.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @param simulate Simulation control. Use `FALSE` (default) for deterministic
#'   Markov model values, `TRUE` for simulation with defaults (`nDraws=10000`,
#'   `seed=NULL`), or a named list like `list(nDraws=5000, seed=12345)`.
#' @return Returns the probability jury finds defendant guilty (if sample_pg is a single number) or vector of such probabilities (if sample_pg is a vector).
#' @description Calculates the probability that a jury of size jury_n finds defendant guilty
#'              given preferences of the jury pool (input as sample_pg).
#'              Does not estimate uncertainty (use as.jury.stats function for inferential statistics).
#' @examples
#'    library(sate)
#'    as.jury.point(sample_pg = .50)
#'
#'    as.jury.point(sample_pg = 10/12)
#'
#'    as.jury.point(sample_pg = .50, simulate = TRUE)
#' @importFrom MASS mvrnorm
#' @export
as.jury.point <- function(sample_pg, jury_n=12, pstrikes=0, dstrikes=0, accuracy=.15, simulate=FALSE)
{
  assert_required(sample_pg)
  assert_between_0_1(sample_pg, accuracy)
  assert_positive_integer(jury_n)
  assert_nonnegative_integer(pstrikes, dstrikes)

  sim_opts <- list(enabled = FALSE, nDraws = 10000L, seed = NULL)
  if (isTRUE(simulate)) {
    sim_opts$enabled <- TRUE
  } else if (isFALSE(simulate)) {
    sim_opts$enabled <- FALSE
  } else if (is.list(simulate)) {
    sim_opts$enabled <- TRUE
    if (length(simulate) > 0L) {
      nm <- names(simulate)
      if (is.null(nm) || any(nm == "")) {
        stop("simulate list entries must be named (allowed: nDraws, seed).", call. = FALSE)
      }
      unknown <- setdiff(nm, c("nDraws", "seed"))
      if (length(unknown) > 0L) {
        stop("simulate has unsupported option(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
      }
      if ("nDraws" %in% nm) sim_opts$nDraws <- simulate$nDraws
      if ("seed" %in% nm) sim_opts$seed <- simulate$seed
    }
  } else {
    stop("simulate must be FALSE, TRUE, or a named list.", call. = FALSE)
  }

  if (sim_opts$enabled) {
    assert_positive_integer(sim_opts$nDraws)
    if (!is.null(sim_opts$seed)) assert_nonnegative_integer(sim_opts$seed)

    simulate_once <- function() {
      # empirical model uncertainty from estimated intercept/slope
      juries_glm_coef = base::c(-6.870279, 11.78409)
      juries_glm_vcov = base::matrix(data=base::c(.08709364, -.143234, -.143234, .24984828), nrow=2, ncol=2)
      coef_draw = MASS::mvrnorm(n=sim_opts$nDraws, mu=juries_glm_coef, Sigma=juries_glm_vcov)
      intercept_draws <- coef_draw[,1]
      slope_draws <- coef_draw[,2]

      jury_PG <- base::rep(NA_real_, length(sample_pg))
      k_scaled <- (0:jury_n/jury_n)
      for(i in seq_along(sample_pg))
      {
        prob_k_given_pg <- select.with.strikes(p_g=sample_pg[i], jury_n=jury_n,
                                               pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
        draw_vals <- base::rep(NA_real_, sim_opts$nDraws)
        for(j in seq_len(sim_opts$nDraws))
        {
          linear_pred = intercept_draws[j] + slope_draws[j] * k_scaled
          jury_PG_by_k <- base::exp(linear_pred) / (1 + base::exp(linear_pred))
          draw_vals[j] <- base::sum(prob_k_given_pg * jury_PG_by_k)
        }
        jury_PG[i] <- mean(draw_vals)
      }

      jury_PG[jury_PG > 1] <- 1
      jury_PG[jury_PG < 0] <- 0
      jury_PG
    }

    return(with_local_seed(sim_opts$seed, simulate_once))
  }

  prob_G_by_k <- get_pG_by_k(jury_n=jury_n)
  jury_PG <- base::rep(NA, length(sample_pg))
  for(i in 1:base::length(sample_pg))
  {
    prob_k_given_pg <- select.with.strikes(p_g=sample_pg[i], jury_n=jury_n,
                                           pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
    jury_PG[i] <- base::sum(prob_k_given_pg*prob_G_by_k)

  }
  jury_PG[jury_PG>1] <- 1
  jury_PG[jury_PG<0] <- 0
  # names(jury_PG) <- "P(G)" # name does not work when sample_pg is vector
  return(jury_PG)
}
