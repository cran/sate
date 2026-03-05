
#' Deliberation function for civil trials (proposed)
#'
#' @param p_votes Initial number of votes for plaintiff.
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns "P" (plaintiff verdict), "D" (defendant verdict), or `NA` if either
#'   input is `NA`.
#' @description The deliberate function returns a jury verdict based on a simulation of deliberation
#'         as a tug-of-war between two verdict factions. The civil version of deliberate does not have
#'         presumption in favor of either party. Can be called directly, but is meant to be called
#'         many times to generate verdict probabilities based on p_votes and jury_n values.
#'         Requires `0 <= p_votes <= jury_n`.
#' @examples
#'    library(sate)
#'    deliberate.civil(p_votes=8, jury_n=12)
#'
#'    deliberate.civil(p_votes=5, jury_n=6)
#' @importFrom stats rbinom
#' @export
deliberate.civil <- function(p_votes, jury_n)
{
  assert_required(p_votes, jury_n)
  if(base::is.na(p_votes) || base::is.na(jury_n)) return(NA)
  if(!base::is.numeric(p_votes) || (p_votes %% 1 != 0) || (p_votes < 0)) base::stop("Initial p_votes must be non-negative integer.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) base::stop("Initial jury_n must be positive integer.")
  if(p_votes > jury_n) base::stop("Initial p_votes must be less than or equal to jury_n.")
  while((p_votes!=0) & (p_votes!=jury_n))
  {
    p_votes <- p_votes - 1 + stats::rbinom(n=1, size=2, prob=.5*(p_votes/(jury_n)) + .25)
    undecided <- 0
  }
  verdict <- base::ifelse(test=(p_votes==jury_n), yes="P", no="D")
  return(verdict)
}
