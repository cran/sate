#' Calculates probability a jury will find defendant guilty based on juror preferences, with standard error and confidence interval
#'
#' @param sample_pg Proportion of jurors who favor a guilty verdict; a number between 0 and 1.
#' @param sample_n Size of sample used to estimate sample_pg.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @param digits Number of digits to report after decimal places; default value is 3.
#' @param simulate Simulation control for uncertainty estimation. Use `FALSE` (default)
#'   for analytic method, `TRUE` for simulation with defaults (`nDraws=10000`,
#'   `seed=NULL`), or a named list like `list(nDraws=5000, seed=12345)`.
#' @return Returns a one-row data.frame with columns: P(G), SE, MOE, Lower 5%, and Upper 95%.
#' @description Calculates probability jury finds defendant guilty based on verdicts preferences of jury pool.
#'              Also reports standard error and confidence interval of estimate (use as.jury.point function for estimate only).
#' @examples
#'    library(sate)
#'    as.jury.stats(sample_pg=.50, sample_n=830)
#'
#'    as.jury.stats(sample_pg=10/12, sample_n=295)
#'
#'    as.jury.stats(sample_pg=.50, sample_n=830, simulate=TRUE)
#'
#'    as.jury.stats(sample_pg=.50, sample_n=830,
#'                  simulate=list(nDraws=5000, seed=12345))
#' @importFrom stats qnorm
#' @export
as.jury.stats <- function(sample_pg, sample_n, jury_n=12,
                          pstrikes=0, dstrikes=0, accuracy=.15, digits=3,
                          simulate=FALSE)
{
  assert_required(sample_pg, sample_n)
  assert_between_0_1(sample_pg, accuracy)
  assert_positive_numeric(sample_n)
  assert_positive_integer(jury_n)
  assert_nonnegative_integer(pstrikes, dstrikes, digits)

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
    return(
      sim.as.jury.stats(
        sample_pg = sample_pg,
        sample_n = sample_n,
        jury_n = jury_n,
        pstrikes = pstrikes,
        dstrikes = dstrikes,
        accuracy = accuracy,
        digits = digits,
        nDraws = sim_opts$nDraws,
        seed = sim_opts$seed
      )
    )
  }

  PG <- as.jury.point(sample_pg, jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  se_pg <- se.prop(p=sample_pg, n=sample_n)
  # print(CI90(m=sample_pg, se=se_pg))
  ci90 <- CI90(m=sample_pg, se=se_pg)
  ci90[ci90>1] <- 1
  ci90[ci90<0] <- 0
  # as.jury.point does not round values
  CI_PG_upper <- as.jury.point(sample_pg=ci90[2], jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  CI_PG_lower <- as.jury.point(sample_pg=ci90[1], jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  CI_PG <- base::c(CI_PG_lower, CI_PG_upper)
  SE <- (CI_PG_upper - CI_PG_lower) / (stats::qnorm(.95) - stats::qnorm(.05))
  MOE <- (CI_PG_upper - CI_PG_lower) / 2
  # print(CI_PG)
  base::names(CI_PG) <- base::c("Lower 5%", "Upper 95%")
  results_table <- data.frame(PG, SE, MOE, CI_PG_lower, CI_PG_upper, row.names = "")
  base::names(results_table) <- base::c("P(G)", "SE", "MOE", "Lower 5%", "Upper 95%")
  return(round(results_table, digits))
}
