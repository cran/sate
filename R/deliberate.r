
#' Deliberation function
#'
#' @param g_votes Initial number of votes for guilty verdict (same as K value).
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns "G" (guilty verdict) or "NG" (not guilty verdict).
#' @description The deliberate function returns a jury verdict based on a simulation of deliberation
#'         as a modified tug-of-war between two verdict factions. Can be called directly, but is
#'         meant to be called many times to generate verdict probabilities based on g_votes and jury_n values.
#' @examples
#'    library(sate)
#'    deliberate(g_votes=10, jury_n=12)
#'
#'    deliberate(g_votes=4, jury_n=6)
#' @importFrom stats rbinom
#' @export
deliberate <- function(g_votes, jury_n)
{
  if(base::is.na(g_votes) || base::is.na(jury_n)) return(NA)
  if(base::missing(g_votes)) stop("Missing initial g_votes.")
  if(base::missing(jury_n)) stop("Missing jury_n value.")
  if(!base::is.numeric(g_votes) || (g_votes %% 1 != 0) || (g_votes < 0)) stop("Initial g_votes must be non-negative integer.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("Initial jury_n must be positive integer.")
  while((g_votes!=0) & (g_votes!=jury_n))
  {
    g_faction <- g_votes - 1
    g_votes <- g_faction + stats::rbinom(n=1, size=2, prob=.5*(g_faction/(jury_n)) + .25)
  }
  verdict <- base::ifelse(test=g_votes==jury_n, yes="G", no="NG")
  return(verdict)
}
