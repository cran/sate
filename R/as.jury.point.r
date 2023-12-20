#' Calculates probability a jury will find defendant guilty based on juror preferences
#'
#' @param sample_pg Proportion of jurors who favor a guilty verdict; a number between 0 and 1.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @return Returns the probability jury finds defendant guilty.
#' @description Calculates probability jury finds defendant guilty based on verdict preferences of jury pool.
#'              Does not estimate uncertainty (use as.jury.stats function for inferential statistics).
#' @examples
#'    library(sate)
#'    as.jury.point(sample_pg = .50)
#'
#'    as.jury.point(sample_pg = 10/12)
#' @export
# @importFrom base is.numeric length missing rep sum
as.jury.point <- function(sample_pg, jury_n=12, pstrikes=0, dstrikes=0, accuracy=.15)
{
  if(base::missing(sample_pg)) stop("Missing sample_pg value.")
  if(!base::is.numeric(sample_pg[1]) || (sample_pg[1] < 0) || (sample_pg[1] > 1)) stop("sample_pg must be number between 0 and 1.")

  prob_G_by_k <- get.model.values(jury_n=jury_n)
  jury_PG <- base::rep(NA, length(sample_pg))
  for(i in 1:base::length(sample_pg))
  {
    prob_k_given_pg <- select.with.strikes(p_g=sample_pg[i], jury_n=jury_n,
                                           pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
    jury_PG[i] <- base::sum(prob_k_given_pg*prob_G_by_k)

  }
  jury_PG[jury_PG>1] <- 1
  jury_PG[jury_PG<0] <- 0
  return(jury_PG)
}
