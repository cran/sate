#' Calculates probability a jury will find defendant guilty based on juror preferences, with standard error and confidence interval
#'
#' @param sample_pg Proportion of jurors who favor a guilty verdict; a number between 0 and 1.
#' @param sample_n Size of sample used to estimate sample_pg.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @param digits Number of digits to report after decimal places; default value is 3.
#' @return Returns the probability jury finds defendant guilty.
#' @description Calculates probability jury finds defendant guilty based on verdicts preferences of jury pool.
#'              Also reports standard error and confidence interval of estimate (use as.jury.point function for estimate only).
#' @examples
#'    library(sate)
#'    as.jury.stats(sample_pg=.50, sample_n=830)
#'
#'    as.jury.stats(sample_pg=10/12, sample_n=295)
# @importFrom base c is.numeric missing names
#' @importFrom stats qnorm
#' @export
as.jury.stats <- function(sample_pg, sample_n, jury_n=12,
                          pstrikes=0, dstrikes=0, accuracy=.15, digits=3)
{
  if(base::missing(sample_pg)) stop("Missing sample_pg value.")
  if(!base::is.numeric(sample_pg) || (sample_pg < 0) || (sample_pg > 1)) stop("sample_pg must be number between 0 and 1.")
  if(base::missing(sample_n)) stop("Missing sample_n value.")
  if(!base::is.numeric(sample_n) || (sample_n <= 0)) stop("Initial sample_n must be positive number")

  PG <- as.jury.point(sample_pg, jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  se_pg <- se.prop(p=sample_pg, n=sample_n)
  # print(CI90(m=sample_pg, se=se_pg))
  ci90 <- CI90(m=sample_pg, se=se_pg)
  ci90[ci90>1] <- 1
  ci90[ci90<0] <- 0
  CI_PG_upper <- as.jury.point(sample_pg=ci90[2], jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  CI_PG_lower <- as.jury.point(sample_pg=ci90[1], jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  CI_PG <- base::c(CI_PG_lower, CI_PG_upper)
  SE <- (CI_PG_upper - CI_PG_lower) / (stats::qnorm(.95) - stats::qnorm(.05))
  MOE <- (CI_PG_upper - CI_PG_lower) / 2
  # print(CI_PG)
  base::names(CI_PG) <- base::c("Lower 5%", "Upper 95%")
  results_table <- round(data.frame(PG, SE, MOE, CI_PG_lower, CI_PG_upper, row.names = ""), digits)
  base::names(results_table) <- base::c("PG", "SE", "MOE", "Lower 5%", "Upper 95%")
  return(results_table)
}
