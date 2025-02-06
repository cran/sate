#' State Demographic Information
#'
#' A dataset with demographic statistics at state level plus national-level that may be used for calculating sample weights.
#' Includes information related to race, educational attainment, household income, age, gender, and ethnicity.
#'
#' @format A data frame with 52 rows and 8 variables.
#' \describe{
#'   \item{state}{Name of state}
#'   \item{StateID}{Two-letter abbreviation for state. USA for nation.}
#'   \item{black}{Proportion of state population who identify as black (African American), per US Census Bureau.}
#'   \item{ba_or_more}{Proportion of adult (18+) population who have attained a BA degree or more, per US Census Bureau.}
#'   \item{hhincome_over50k}{Proportion of state population with household income of $50,000 or more, per US Census Bureau.}
#'   \item{age35plus}{Proportion of adult (18+) population age 35 or older, per US Census Bureau.}
#'   \item{woman}{Proportion of state population who identify as women, per US Census Bureau.}
#'   \item{hispanic}{Proportion of state population who identify as Hispanic, per US Census Bureau. }
#' }
#' @source U.S. Census Bureau, American Community Survey, 5-Year Estimates.
#' @export
"state.demographic.info"
