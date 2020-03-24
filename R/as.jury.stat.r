#' Calculates jury-level statistics
#'
#' @param pg The proportion of jurors who favor a guilty verdict.
#' @param n The size of the sample used to estimate pg.
#' @param seed Set seed for random number generation for replication, default is NULL (optional).
#' @param ndraw The number of simulations used to generate results. Should be very large number (default = 100000).
#' @return Returns a list of jury-level statistics
#' @description Calculates jury-level statistics based on user-defined inputs.
#' @examples
#'    as.jury.stat(pg=.50, n=500)
#'
#'    as.jury.stat(pg=10/12, n=1200, seed=123, ndraw=10000)
#' @export
#' @import MASS

as.jury.stat = function(pg, n, seed=NULL, ndraw=100000) {

  set.seed(seed) # set seed to be able to replicate exact numbers

  nDraws <- ndraw
  juries_glm_coef = c(-6.870279, 11.78409)
  juries_glm_vcov = matrix(data=c(.08709364, -.143234, -.143234, .24984828), nrow=2, ncol=2)
  coef_draw = MASS::mvrnorm(n=nDraws,
                            mu=juries_glm_coef,
                            Sigma=juries_glm_vcov)
  intercept_draws <- coef_draw[,1]
  slope_draws <- coef_draw[,2]
  pg_se <- sqrt(pg*(1-pg)/n)
  pg_draws = stats::rnorm(nDraws, pg, pg_se)
  linear_pred = intercept_draws + slope_draws*pg_draws
  jury_PG_estimates <- exp(linear_pred) / (1 + exp(linear_pred))
  CI = stats::quantile(jury_PG_estimates, probs=c(.05, .95))

  return(list(PG=mean(jury_PG_estimates), SE=stats::sd(jury_PG_estimates), CI90=CI))
}
