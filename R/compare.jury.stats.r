
#' Estimates jury-level differences based on juror-level statistics with inferential
#' statistics
#'
#' @param pg_actual The proportion of jurors who favor a guilty verdict in the
#'                  actual trial condition (the trial with error).
#' @param n_actual The size of the sample used to estimate pg_actual.
#' @param pg_hypo The proportion of jurors who favor a guilty verdict in the
#'                hypothetical trial condition (the fair trial without error).
#' @param n_hypo The size of the sample used to estimate pg_hypo.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1;
#'                 default value is .15.
#' @param digits Number of digits to report after decimal places; default value is 3.
#' @param simulate Simulation control for uncertainty estimation. Use `FALSE` (default)
#'   for analytic method, `TRUE` for simulation with defaults (`nDraws=10000`,
#'   `seed=NULL`), or a named list like `list(nDraws=5000, seed=12345)`.
#' @return Returns a list of jury-level statistics to assess effect of a trial error.
#'         Returned list includes statistics for actual jury, hypothetical jury, and
#'         the difference between them.
#' @description Calculates jury-level statistics and differences based on juror-level
#'              statistics supplied by user.
#' @examples
#'    library(sate)
#'    compare.jury.stats(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)
#'
#'    compare.jury.stats(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
#'                       jury_n=6, pstrikes=3, dstrikes=3)
#'
#'    compare.jury.stats(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450,
#'                       simulate=TRUE)
#' @export
compare.jury.stats = function(pg_actual, n_actual, pg_hypo, n_hypo, jury_n=12,
                              pstrikes=0, dstrikes=0, accuracy=.15, digits=3,
                              simulate=FALSE)
{
  assert_required(pg_actual, n_actual, pg_hypo, n_hypo)
  assert_between_0_1(pg_actual, pg_hypo, accuracy)
  assert_positive_numeric(n_actual, n_hypo)
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
      sim.compare.jury.stats(
        pg_actual = pg_actual,
        n_actual = n_actual,
        pg_hypo = pg_hypo,
        n_hypo = n_hypo,
        jury_n = jury_n,
        digits = digits,
        pstrikes = pstrikes,
        dstrikes = dstrikes,
        accuracy = accuracy,
        seed = sim_opts$seed,
        nDraws = sim_opts$nDraws
      )
    )
  }

  actual_jury_stats <- as.jury.stats(sample_pg=pg_actual, sample_n=n_actual,
                                     jury_n=jury_n, digits=99, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)

  hypo_jury_stats <- as.jury.stats(sample_pg=pg_hypo, sample_n=n_hypo,
                                   jury_n=jury_n, digits=99, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)

  PG_diff = get_pg_value(actual_jury_stats, "actual_jury_stats") -
    get_pg_value(hypo_jury_stats, "hypo_jury_stats")
  SE_diff = base::sqrt(actual_jury_stats$SE^2 + hypo_jury_stats$SE^2)
  CI_diff = CI90(m=PG_diff, se=SE_diff)
  MOE = (CI_diff[2] - CI_diff[1]) / 2
  base::names(CI_diff) <- base::c("Lower 5%", "Upper 95%")

  difference_table <- data.frame(PG_diff, SE_diff, MOE, CI_diff[1], CI_diff[2], row.names = "")
  colnames(difference_table) <- base::c("Difference in P(G)", "SE", "MOE", "Lower 5%", "Upper 95%")

  # names added for display purposes
  names(actual_jury_stats) <- c("P(G|actual)", "SE", "MOE", "Lower 5%", "Upper 95%")
  names(hypo_jury_stats) <- c("P(G|hypo)", "SE", "MOE", "Lower 5%", "Upper 95%")

  return(base::list(actual_jury=round(actual_jury_stats, digits),
                    hypo_jury=round(hypo_jury_stats, digits),
                    difference=round(difference_table, digits)))
}
