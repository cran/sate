
#' Estimates jury-level probability of guilty verdict based on juror-level statistics based on empirical data
#'
#' @param sample_pg The proportion of jurors who favor a guilty verdict in the jury pool
#' @param sample_n The size of the sample used to estimate sample_pg
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @param digits Number of digits to report after decimal places; default value is 3.
#' @param nDraws The number of simulations used to generate results. Should be very large number (default = 10000).
#' @param seed Set seed for random number generation for replication, default is 12345.
#' @return Returns a list of jury-level statistics to assess effect of a trial error.
#' @description Returns estimate of the probability of guilty verdict based on juror-level
#'              statistics supplied by user. Also reports inferential statistics. Results
#'              are based on an empirical model with greater uncertainty than as.jury.stats
#'              function.
#' @examples
#'    library(sate)
#'    sim.as.jury.stats(sample_pg=.50, sample_n=830, nDraws=500)
#'
#'    sim.as.jury.stats(sample_pg=10/12, sample_n=295, pstrikes=6, dstrikes=10, nDraws=1000)
#' @export
# @importFrom base c cat exp is.numeric list matrix missing names rep round set.seed sum
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
sim.as.jury.stats = function(sample_pg, sample_n, jury_n=12,
                             pstrikes=0, dstrikes=0, accuracy=.15, digits=3,
                             nDraws=10000, seed=12345)
{
  if(base::missing(sample_pg)) stop("Missing sample_pg value.")
  if(!base::is.numeric(sample_pg) || (sample_pg < 0) || (sample_pg > 1)) stop("sample_pg must be number between 0 and 1.")
  if(base::missing(sample_n)) stop("Missing sample_n value.")
  if(!base::is.numeric(sample_n) || (sample_n <= 0)) stop("sample_n must be positive number.")
  if(nDraws > 10000) base::cat("This function is computationally intensive. Please be patient.\n\n")

  base::set.seed(seed) # set seed to be able to replicate exact numbers
  # coef and vcov based on empircal analysis
  juries_glm_coef = base::c(-6.870279, 11.78409)
  juries_glm_vcov = base::matrix(data=base::c(.08709364, -.143234, -.143234, .24984828), nrow=2, ncol=2)

  # linear_pred_point = juries_glm_coef[1] + juries_glm_coef[2]*sample_pg
  # jury_PG_point_est <- exp(linear_pred_point) / (1 + exp(linear_pred_point))
  # vector of pg_draws for jury pool based on sample_pg
  pg_draws = stats::rnorm(nDraws, sample_pg, se.prop(p=sample_pg, n=sample_n))
  pg_draws[pg_draws < 0] <- 0
  pg_draws[pg_draws > 1] <- 1
  # vectors of model slopes and intercepts give estimation uncertainty
  coef_draw = MASS::mvrnorm(n=nDraws, mu=juries_glm_coef, Sigma=juries_glm_vcov)
  intercept_draws <- coef_draw[,1]
  slope_draws <- coef_draw[,2]
  jury_PG_estimates <- base::rep(NA, nDraws)

  for(i in 1:nDraws)
  {
    prob_k_given_pg <- select.with.strikes(p_g=pg_draws[i], jury_n=jury_n,
                                           pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
    linear_pred = intercept_draws[i] + slope_draws[i]*(0:jury_n/jury_n)
    jury_PG_by_k <- base::exp(linear_pred) / (1 + base::exp(linear_pred))
    jury_PG_estimates[i] <- base::sum(prob_k_given_pg*jury_PG_by_k)
  }

  PG <- mean(jury_PG_estimates)
  CI90 <- stats::quantile(x = jury_PG_estimates, probs = base::c(.05, .95))
  SE <- base::as.numeric((CI90[2] - CI90[1]) / (qnorm(.95) - qnorm(.05)))
  MOE <- base::as.numeric((CI90[2] - CI90[1]) / 2)
  CI_PG_lower <- CI90[1]
  CI_PG_upper <- CI90[2]
  # base::names(CI90) <- base::c("Lower 90% CI", "Upper 90% CI")

  # return(base::list(PG=base::round(mean(jury_PG_estimates), digits),
  #             SE=base::round(se_PG_estimates, digits),
  #             CI=base::round(CI90, digits)))

  results_table <- round(data.frame(PG, SE, MOE, CI_PG_lower, CI_PG_upper, row.names = ""), digits)
  base::names(results_table) <- base::c("PG", "SE", "MOE", "Lower 5%", "Upper 95%")
  return(results_table)

}

