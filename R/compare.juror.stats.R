#' Estimates juror-level differences based on sample statistics (from survey)
#'
#' @param pg_actual The proportion of jurors who favor a guilty verdict in the
#'                  actual trial condition (the trial with error).
#' @param n_actual The size of the sample used to estimate pg_actual.
#' @param pg_hypo The proportion of jurors who favor a guilty verdict in the
#'                hypothetical trial condition (the fair trial without error).
#' @param n_hypo The size of the sample used to estimate pg_hypo.
#' @param digits Number of digits to report after decimal places; default value is 3.
#' @return Returns a list of juror-level statistics to assess the effect of a trial error or omission on juror preferences.
#'         Returned list includes statistics for the actual trial, the hypothetical trial, and
#'         the difference between them.
#' @description Calculates juror-level statistics and differences based on sample
#'              statistics (from a survey) supplied by user.
#' @examples
#'    library(sate)
#'    compare.juror.stats(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)
#'
#'    compare.juror.stats(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350)
#' @export
compare.juror.stats <- function(pg_actual, n_actual, pg_hypo, n_hypo, digits=3)
{

  se_pg_actual <- se.prop(pg_actual, n_actual)
  CI90 <- CI90(pg_actual, se_pg_actual)
  actual_jurors <- base::round(base::c(pg = pg_actual, SE = se_pg_actual,
                           MOE = se_pg_actual*qnorm(.95),
                           Lower_5 = CI90[1],
                           Upper_95 = CI90[2]), digits)
  names(actual_jurors) <- c("P(g|actual)", "SE", "MOE", "Lower 5%", "Upper 95%")

  se_pg_hypo <- se.prop(pg_hypo, n_hypo)
  CI90 <- CI90(pg_hypo, se_pg_hypo)
  hypo_jurors <- base::round(base::c(pg = pg_hypo, SE = se_pg_hypo,
                         MOE = se_pg_hypo*qnorm(.95),
                         Lower_5 = CI90[1],
                         Upper_95 = CI90[2]), digits)
  names(hypo_jurors) <- c("P(g|hypo)", "SE", "MOE", "Lower 5%", "Upper 95%")

  se_pg_diff <- sqrt(se_pg_actual^2 + se_pg_hypo^2)
  CI90 <- CI90(pg_actual - pg_hypo, se_pg_diff)
  difference_jurors <- base::round(base::c(diff = pg_actual - pg_hypo, SE = se_pg_diff,
                               MOE = se_pg_diff*qnorm(.95),
                               Lower_5 = CI90[1],
                               Upper_95 = CI90[2]), digits)
  names(difference_jurors) <- c("Difference", "SE", "MOE", "Lower 5%", "Upper 95%")
  # PG    SE   MOE Lower 5% Upper 95%
  return(list(actual_trial=actual_jurors, hypo_trial=hypo_jurors, difference=difference_jurors))
}
