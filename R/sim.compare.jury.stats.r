
#' Estimates jury-level differences based on juror-level statistics using simulations based on empirical data
#'
#' @param pg_actual The proportion of jurors who favor a guilty verdict in the actual trial condition (the trial with error).
#' @param n_actual The size of the sample used to estimate pg_actual.
#' @param pg_hypo The proportion of jurors who favor a guilty verdict in the hypothetical trial condition (the fair trial without error).
#' @param n_hypo The size of the sample used to estimate pg_hypo.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param digits Number of digits to report after decimal places; default value is 3.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @param seed Optional non-negative integer seed for replication. Default is NULL (no seeding).
#' @param nDraws The number of simulations used to generate results. Should be very large number (default = 10000).
#' @return Returns a list of jury-level statistics to assess effect of a trial error.
#' @description Calculates jury-level differences based on juror-level statistics supplied by user.
#'              Results based on empirical data, inferential statistics produced via simulations.
#' @examples
#'    library(sate)
#'    sim.compare.jury.stats(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450, nDraws=500)
#'
#'    sim.compare.jury.stats(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
#'                       seed=12345, nDraws=1000)
#' @export
sim.compare.jury.stats = function(pg_actual, n_actual, pg_hypo, n_hypo, jury_n=12, digits=3,
                                  pstrikes=0, dstrikes=0, accuracy=.15, seed=NULL, nDraws=10000)
{
  assert_required(pg_actual, n_actual, pg_hypo, n_hypo)
  assert_between_0_1(pg_actual, pg_hypo, accuracy)
  assert_positive_numeric(n_actual, n_hypo)
  assert_positive_integer(jury_n, nDraws)
  assert_nonnegative_integer(pstrikes, dstrikes, digits)
  if (!is.null(seed)) assert_nonnegative_integer(seed)
  # cat("This function is computationally intensive. Please be patient.\n")

  actual_jury_sim_stats <- sim.as.jury.stats(sample_pg=pg_actual, sample_n=n_actual,
                                             jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy,
                                             digits=digits, nDraws=nDraws, seed=seed)
  hypo_jury_sim_stats   <- sim.as.jury.stats(sample_pg=pg_hypo, sample_n=n_hypo,
                                             jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy,
                                             digits=digits, nDraws=nDraws, seed=seed)
  PG_diff = get_pg_value(actual_jury_sim_stats, "actual_jury_sim_stats") -
    get_pg_value(hypo_jury_sim_stats, "hypo_jury_sim_stats")
  SE_diff = base::sqrt(actual_jury_sim_stats$SE^2 + hypo_jury_sim_stats$SE^2)
  CI_diff = CI90(m=PG_diff, se=SE_diff)
  MOE <- base::as.numeric((CI_diff[2] - CI_diff[1]) / 2)
  base::names(CI_diff) <- base::c("Lower 5%", "Upper 95%")

  difference_table <- data.frame(PG_diff, SE_diff, MOE, CI_diff[1], CI_diff[2], row.names = "")
  colnames(difference_table) <- base::c("Difference in P(G)", "SE", "MOE", "Lower 5%", "Upper 95%")

  # names added for display purposes
  names(actual_jury_sim_stats) <- c("P(G|actual)", "SE", "MOE", "Lower 5%", "Upper 95%")
  names(hypo_jury_sim_stats) <- c("P(G|hypo)", "SE", "MOE", "Lower 5%", "Upper 95%")

  return(base::list(actual_jury=round(actual_jury_sim_stats, digits),
                    hypo_jury=round(hypo_jury_sim_stats, digits),
                    difference=round(difference_table, digits)))

}

