#' Dataset of Observed Deliberations
#'
#' A compilation of observed jury deliberations from multiple studies used to analyze
#' relationship between initial state of jury and probability of verdict.
#'
#' @format A data frame with 2382 rows and 7 variables.
#' \describe{
#'   \item{idnum}{Internal identification number.}
#'   \item{prop_jurors_g}{Proportion of jurors initially in favor of guilty/death verdict.}
#'   \item{jury_size}{Size of jury.}
#'   \item{guilty_verdict}{Did jury render guilty/death verdict? 1 = yes, 0 = no.}
#'   \item{six_person_jury}{Deliberation by six-person jury? 1 = yes, 0 = no.}
#'   \item{death_penalty}{Was jury deliberating death penalty?  1 = yes, 0 = no.}
#'   \item{source}{Source of data. Devine_2001 = Devine et al. (2001) table 6 without Sandys & Dillehey (1995);
#'                                 Devine_2004 = Devine et al. (2004) table 2;
#'                                 Devine_2007 = Devine et al. (2007) with correction for undecideds suggested by Kerr and McCoun (2012);
#'                                 Sandys_1995 = Sandys & Dillehey (1995) with correction for undecideds suggested by Kerr and McCoun (2012);
#'                                 CJP_2015 = Capital Jury Project from Devine & Kelly (2015), some imputed prop_jurors_g values;
#'                                 NCSC_LA = Hannaford-Agor et al. (2001), NCSC Study, Los Angeles site trials, with identifying number;
#'                                 NCSC_AZ = Hannaford-Agor et al. (2001), NCSC Study, Maricopa site trials, with identifying number;
#'                                 NCSC_NY = Hannaford-Agor et al. (2001), NCSC Study, Bronx site trials, with identifying number;
#'                                 NCSC_DC = Hannaford-Agor et al. (2001), NCSC Study, Washington, DC site trials, with identifying number; }
#' }
#' @source Compilation of multiple sources, see source variable.
"observed.deliberations"

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
