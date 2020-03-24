#' Estimates jury-level differences based on juror-level statistics
#'
#' @param pg_actual The proportion of jurors who favor a guilty verdict in the actual trial condition (the trial with error).
#' @param n_actual The size of the sample used to estimate pg_actual.
#' @param pg_hypo The proportion of jurors who favor a guilty verdict in the hypothetical trial condition (the fair trial without error).
#' @param n_hypo The size of the sample used to estimate pg_hypo.
#' @param seed Set seed for random number generation for replication, default is NULL (optional).
#' @param ndraw The number of simulations used to generate results. Should be very large number (default = 1000000).
#' @return Returns a list of jury-level statistics to assess effect of a trial error.
#' @description Calculates jury-level differences based on juror-level statistics supplied by user.
#' @examples
#'    compare.jury.stats(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)
#'
#'    compare.jury.stats(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
#'                       seed=12345, ndraw=100000)
#' @export
#' @import MASS

compare.jury.stats <- function(pg_actual, n_actual, pg_hypo, n_hypo, seed=NULL, ndraw=1000000) {

  nDraws = ndraw
  set.seed(seed) # set seed to be able to replicate exact numbers

  juries_glm_coef = c(-6.870279, 11.78409)
  juries_glm_vcov = matrix(data=c(.08709364, -.143234, -.143234, .24984828), nrow=2, ncol=2)


  coef_draw_actual = MASS::mvrnorm(n=nDraws,
                             mu=juries_glm_coef,
                             Sigma=juries_glm_vcov)
  intercept_draws_actual = coef_draw_actual[,1]
  slope_draws_actual = coef_draw_actual[,2]
  pg_se_actual = sqrt(pg_actual*(1-pg_actual)/n_actual)
  pg_draws_actual = stats::rnorm(nDraws, pg_actual, pg_se_actual)
  linear_pred_actual = intercept_draws_actual + slope_draws_actual*pg_draws_actual
  jury_PG_actual_estimates = exp(linear_pred_actual) / (1 + exp(linear_pred_actual))
  CI_actual = stats::quantile(jury_PG_actual_estimates, probs=c(.05, .95))

  coef_draw_hypo = MASS::mvrnorm(n=nDraws,
                           mu=juries_glm_coef,
                           Sigma=juries_glm_vcov)
  intercept_draws_hypo = coef_draw_hypo[,1]
  slope_draws_hypo = coef_draw_hypo[,2]
  pg_se_hypo = sqrt(pg_hypo*(1-pg_hypo)/n_hypo)
  pg_draws_hypo = stats::rnorm(nDraws, pg_hypo, pg_se_hypo)
  linear_pred_hypo = intercept_draws_hypo + slope_draws_hypo*pg_draws_hypo
  jury_PG_hypo_estimates = exp(linear_pred_hypo) / (1 + exp(linear_pred_hypo))
  CI_hypo = stats::quantile(jury_PG_hypo_estimates, probs=c(.05, .95))

  diff_PG_estimates = jury_PG_actual_estimates - jury_PG_hypo_estimates
  CI_diff = stats::quantile(diff_PG_estimates, probs=c(.05, .95))

  # jurors_diff = pg_actual - pg_hypo
  # jurors_diff_se = sqrt(pg_se_actual^2 + pg_se_hypo^2)

  return(list(PG_actual=mean(jury_PG_actual_estimates), SE_actual=stats::sd(jury_PG_actual_estimates), CI_actual=CI_actual,
              PG_hypo=mean(jury_PG_hypo_estimates), SE_hypo=stats::sd(jury_PG_hypo_estimates), CI_hypo=CI_hypo,
              PG_diff=mean(diff_PG_estimates), SE_diff=stats::sd(diff_PG_estimates), CI_diff=CI_diff)
  )
}
