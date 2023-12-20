
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
#' @param seed Set seed for random number generation for replication, default is 12345.
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
# @importFrom base c is.numeric list missing names round sqrt
sim.compare.jury.stats = function(pg_actual, n_actual, pg_hypo, n_hypo, jury_n=12, digits=3,
                                  pstrikes=0, dstrikes=0, accuracy=.15, seed=12345, nDraws=10000)
{
  if(base::missing(pg_actual)) stop("Missing pg_actual value.")
  if(!base::is.numeric(pg_actual) || (pg_actual < 0) || (pg_actual > 1)) stop("pg_actual must be number between 0 and 1.")
  if(base::missing(pg_hypo)) stop("Missing pg_hypo value.")
  if(!base::is.numeric(pg_hypo) || (pg_hypo < 0) || (pg_hypo > 1)) stop("pg_hypo must be number between 0 and 1.")
  if(base::missing(n_actual)) stop("Missing n_actual value.")
  if(!base::is.numeric(n_actual) || (n_actual <= 0)) stop("n_actual must be positive number.")
  if(base::missing(n_hypo)) stop("Missing n_hypo value.")
  if(!base::is.numeric(n_hypo) || (n_hypo <= 0)) stop("n_hypo must be positive number.")
  # cat("This function is computationally intensive. Please be patient.\n")

  actual_jury_sim_stats <- sim.as.jury.stats(sample_pg=pg_actual, sample_n=n_actual,
                                             jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy,
                                             digits=digits, nDraws=nDraws, seed=seed)
  hypo_jury_sim_stats   <- sim.as.jury.stats(sample_pg=pg_hypo, sample_n=n_hypo,
                                             jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy,
                                             digits=digits, nDraws=nDraws, seed=seed)
  PG_diff = actual_jury_sim_stats$PG - hypo_jury_sim_stats$PG
  SE_diff = base::sqrt(actual_jury_sim_stats$SE^2 + hypo_jury_sim_stats$SE^2)
  CI_diff = CI90(m=PG_diff, se=SE_diff)
  base::names(CI_diff) <- base::c("Lower 5%", "Upper 95%")
  return(base::list(actual_jury=actual_jury_sim_stats,
              hypo_jury=hypo_jury_sim_stats,
              PG_diff=base::round(PG_diff, digits),
              SE_diff=base::round(SE_diff, digits),
              CI_diff=base::round(CI_diff, digits)))

}
