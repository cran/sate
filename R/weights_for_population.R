#' Calculates survey weights given respondent information and target population demographics
#'
#' @param respondentdata Dataset with encoded respondent demographic information (use encode.cloud.respondent.variables to prepare respondentdata) must have a ParticipantId variable.
#' @param targetdata A one row data.frame (or named vector) with the following statistics: black, ba_or_more, hhincome_over50k, age35plus, woman, hispanic (use target.population.demographics to obtain)
#' @return Returns respondentdata with raked sampling weights encoded.
#' @description Calculates survey weights given respondent information and target population demographics. Respondent
#'              demographic info must be properly encoded in respondentdata to work with the target.demographics. If
#'              respondent demographic info is missing, the respondent's weight will be coded 1. Weight values trimmed
#'              so that no weights are greater than 6 or less than .1.
#' @examples
#'    library(sate)
#'
#'    example_n <- 100
#'    example <- data.frame(Race = sample(x=c("Black or African American", "Other"),
#'                                        size=example_n, replace=TRUE),
#'                          Education = sample(x=c("Bachelor's degree (for example: BA, AB, BS)",
#'                                             "Other"), size=example_n, replace=TRUE),
#'                          Household.Income = sample(x=c("$70,000-$79,999", "Other"),
#'                                                    size=example_n, replace=TRUE),
#'                          Age = sample(x=18:80, size=example_n, replace=TRUE),
#'                          Gender = sample(x=c("Woman", "Man", "Prefer not to say"),
#'                                          size=example_n, replace=TRUE),
#'                          Ethnicity = sample(x=c("No, not of Hispanic, Latino, or Spanish origin",
#'                                             "Other"), size=example_n, replace=TRUE),
#'                          ParticipantId = 1:example_n)
#'    respondents.encoded <- encode.cloud.respondent.variables(dataset=example)
#'
#'    pop.targets <- target.population.demographics(state="FL")
#'
#'    respondents.weighted <- weights_for_population(respondentdata = respondents.encoded,
#'                                                   targetdata = pop.targets)
#'
#' @export
#' @importFrom stats weights
#' @importFrom survey rake svydesign trimWeights
#'
weights_for_population <- function(respondentdata, targetdata)
{
  pop.black <- targetdata$black
  pop.ba_or_more <- targetdata$ba_or_more
  pop.hhincome_over50k <- targetdata$hhincome_over50k
  pop.age35plus <- targetdata$age35plus
  pop.woman <- targetdata$woman
  pop.hispanic <- targetdata$hispanic

  respondentdata.all <- respondentdata
  respondentdata.svy.unweighted <- survey::svydesign(ids=~1, weights=~1,
                                                     data=subset(respondentdata.all, respondentdata.all$respondent_na=="FALSE"))
  nrows <- base::nrow(respondentdata.svy.unweighted$variables)

  # population targets multipled by nrow(respondentdata.all)
  # gives the target distribution for the respondentdata
  target.black.dist <- base::data.frame(black = c("TRUE", "FALSE"), Freq = nrows * c(pop.black, 1 - pop.black))
  target.ba_or_more.dist <- base::data.frame(ba_or_more = c("TRUE", "FALSE"), Freq = nrows * c(pop.ba_or_more, 1 - pop.ba_or_more))
  target.hhincome_over50k.dist <- base::data.frame(hhincome_over50k = c("TRUE", "FALSE"), Freq = nrows * c(pop.hhincome_over50k, 1 - pop.hhincome_over50k))
  target.age35plus.dist <- base::data.frame(age35plus = c("TRUE", "FALSE"), Freq = nrows * c(pop.age35plus, 1 - pop.age35plus))
  target.woman.dist <- base::data.frame(woman = c("TRUE", "FALSE"), Freq = nrows * c(pop.woman, 1 - pop.woman))
  target.hispanic.dist <- base::data.frame(hispanic = c("TRUE", "FALSE"), Freq = nrows * c(pop.hispanic, 1 - pop.hispanic))

  respondentdata.svy.rake <- survey::rake(design = respondentdata.svy.unweighted,
                                  sample.margins = base::list(~black, ~ba_or_more, ~hhincome_over50k, ~age35plus, ~woman, ~hispanic),
                                  population.margins = base::list(target.black.dist, target.ba_or_more.dist, target.hhincome_over50k.dist,
                                                            target.age35plus.dist, target.woman.dist, target.hispanic.dist))

  respondentdata.svy.rake <- survey::trimWeights(respondentdata.svy.rake, upper = 6, lower = .1)
  # there seem to be different rules of thumb but it seems to be a good quality control

  # the rake function can't handle missing data, so need to store separately and then merge
  weights_data <- base::data.frame(weights = stats::weights(respondentdata.svy.rake), ParticipantId = respondentdata.svy.rake$variables$ParticipantId)

  ##### check this part, it may need to come later or be broken into parts
  merged_respondentdata <- base::merge(respondentdata.all, weights_data, by = base::c("ParticipantId"), all.x=TRUE)
  respondentdata.all <- merged_respondentdata # should add one variable (weights) to respondentdata

  # if weight not calculated (respondent info missing), give respondent mean weight
  respondentdata.all$weights[base::is.na(respondentdata.all$weights)] <- base::mean(respondentdata.all$weights, na.rm=T)
  # make sure mean weight = 1
  respondentdata.all$weights <- respondentdata.all$weights / base::mean(respondentdata.all$weights, na.rm=T)
  return(respondentdata.all)
}

