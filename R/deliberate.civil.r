
#' Deliberation function for civil trials (proposed)
#'
#' @param p_votes Initial number of votes for plaintiff.
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns "P" (plaintiff verdict) or "D" (defendant verdict).
#' @description The deliberate function returns a jury verdict based on a simulation of deliberation
#'         as a tug-of-war between two verdict factions. The civil version of deliberate does not have
#'         presumption in favor of either party. Can be called directly, but is meant to be called
#'         many times to generate verdict probabilities based on p_votes and jury_n values.
#' @examples
#'    library(sate)
#'    deliberate.civil(p_votes=8, jury_n=12)
#'
#'    deliberate.civil(p_votes=5, jury_n=6)
#' @importFrom stats rbinom
#' @export
deliberate.civil <- function(p_votes, jury_n)
{
  if(base::missing(p_votes)) base::message("Missing initial p_votes.")
  if(base::missing(jury_n)) base::message("Missing jury_n value.")
  if(!base::is.numeric(p_votes) || (p_votes %% 1 != 0) || (p_votes < 0)) base::message("Initial p_votes must be non-negative integer.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) base::message("Initial jury_n must be positive integer.")
  while((p_votes!=0) & (p_votes!=jury_n))
  {
    p_votes <- p_votes - 1 + stats::rbinom(n=1, size=2, prob=.5*(p_votes/(jury_n)) + .25)
    undecided <- 0
  }
  verdict <- base::ifelse(test=(p_votes==jury_n), yes="P", no="D")
  return(verdict)
}
