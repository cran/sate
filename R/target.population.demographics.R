#' Looks up and returns key demographic statistics for target state to be used for calculating sample weights
#'
#' @param state The target state, input as two-letter abbreviation (i.e., "GA" "TX" or "FL"). If no state specified, will use "USA".
#' @return A one row data.frame with the following statistics: black, ba_or_more, hhincome_over50k, age35plus, woman, hispanic
#' @description Looks up and returns six key demographic statistics for a target state to be used for calculating sample weights.
#'              State-level population statistics from U.S. Census Bureau, American Community Survey 5-year estimates.
#'              Data from state.demographic.info, a saved datafile in sate package.
#' @examples
#'    library(sate)
#'    target.population.demographics(state="FL")
#'
#'    target.population.demographics()   # will return stats for USA
#'
#' @export
target.population.demographics <- function(state)
{
  if(missing(state))
  {
    state <- "USA"
    base::message("No state specified, using USA as target population.")
  }
  target.pop.info <- state.demographic.info[state.demographic.info$StateID==state,
                                            c("black", "ba_or_more", "hhincome_over50k", "age35plus", "woman", "hispanic")]
  return(target.pop.info)
}
