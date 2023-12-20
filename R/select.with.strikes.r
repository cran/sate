
#' Generates the distribution of initial votes for guilty verdict on juries
#'
#' @param p_g The proportion of jurors in the jury pool who favor a guilty verdict
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @return A vector of probabilities for 0:jury_n initial guilty votes
#' @description Calculates and returns probability distribution of initial votes for guilty verdict
#'              from 0:jury_n with options for peremptory strikes and strike accuracy
#' @examples
#'    library(sate)
#'    select.with.strikes(p_g=.70, jury_n=6)
#'
#'    select.with.strikes(p_g=.75, jury_n=12, pstrikes=6, dstrikes=10)
#'
# @importFrom base is.numeric matrix missing rep sum
#' @importFrom stats dbinom
#' @export
select.with.strikes = function(p_g, jury_n=12, pstrikes=0, dstrikes=0, accuracy=.15)
{
  if(base::missing(p_g)) stop("Missing p_g value.")
  if(!base::is.numeric(p_g) || (p_g < 0) || (p_g > 1)) stop("p_g must be number between 0 and 1.")
  if(!base::is.numeric(accuracy) || (accuracy < 0) || (accuracy > 1)) stop("accuracy must be number between 0 and 1.")

  d_hit_prob = stats::dbinom(x = 0:dstrikes, size = dstrikes, prob = accuracy)
  p_hit_prob = stats::dbinom(x = 0:pstrikes, size = pstrikes, prob = accuracy)
  probability <- base::rep(NA, (dstrikes+1)*(pstrikes+1))
  counter <- 1
  jury_distributions <- base::matrix(ncol = (jury_n+1), nrow = (dstrikes+1)*(pstrikes+1), byrow = T)
  for(d in 1:(dstrikes+1))
  {
    for(p in 1:(pstrikes+1))
    {
      probability[counter] <- d_hit_prob[d] * p_hit_prob[p]
      p_hits <- p - 1
      d_hits <- d - 1
      initial_n <- jury_n + p_hits + d_hits
      expected_initial <- stats::dbinom(x = 0:initial_n, size = initial_n, prob = p_g)
      after_strikes <- base::rep(0, jury_n+1)
      after_strikes[1] <- base::sum(expected_initial[1:(d_hits+1)])
      after_strikes[2:jury_n] <- expected_initial[(d_hits+2):(initial_n - p_hits)]
      after_strikes[jury_n+1] <- base::sum(expected_initial[(initial_n - p_hits + 1):(initial_n + 1)])
      jury_distributions[counter,] <- after_strikes
      counter <- counter + 1
    }
  }
  result <- probability %*% jury_distributions
  return(result)
}
