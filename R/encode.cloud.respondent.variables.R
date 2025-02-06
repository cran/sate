#' Encodes Cloud Research respondent information in form suitable for calculating sampling weights
#'
#' @param dataset Dataset containing Cloud Research respondent demographic information
#' @return Returns dataset with encoded variables added: black, ba_or_more, hhincome_over50k, age35plus, woman, hispanic, and respondent_na.
#' @description Encodes Cloud research respondent information with names and values suitable
#'              for calculating sampling weights. All variables encoded and added to dataset are booleans. The variable respondent_na is TRUE
#'              if the respondent's information is "Prefer not to say" or missing on any variable.
#' @examples
#'    library(sate)
#'
#'    example <- data.frame(Race = sample(x=c("Black or African American", "Other"),
#'                                        size=10, replace=TRUE),
#'                          Education = sample(x=c("Bachelor's degree (for example: BA, AB, BS)",
#'                                             "Other"), size=10, replace=TRUE),
#'                          Household.Income = sample(x=c("$70,000-$79,999", "Other"),
#'                                                    size=10, replace=TRUE),
#'                          Age = sample(x=18:80, size=10, replace=TRUE),
#'                          Gender = sample(x=c("Woman", "Man", "Prefer not to say"),
#'                                          size=10, replace=TRUE),
#'                          Ethnicity = sample(x=c("No, not of Hispanic, Latino, or Spanish origin",
#'                                             "Other"), size=10, replace=TRUE))
#'    dataset.encoded <- encode.cloud.respondent.variables(dataset=example)
#'
#' @export
#'
encode.cloud.respondent.variables <- function(dataset)
{
  respondentdata <- dataset
  # weights based on list(pop.black, pop.ba_or_more, pop.hhincome_over50k, pop.age35plus, pop.woman, pop.hispanic))

  # Race
  respondentdata$Race[respondentdata$Race=="Prefer not to say"] <- NA
  respondentdata$black <- (respondentdata$Race == "Black or African American")

  # Educational attainment
  respondentdata$Education[respondentdata$Education=="Prefer not to say"] <- NA
  respondentdata$ba_or_more <- (respondentdata$Education == "Bachelor's degree (for example: BA, AB, BS)" |
                                  respondentdata$Education == "Master's degree (for example: MA, MS, MEng, MEd, MSW, MBA)" |
                                  respondentdata$Education == "Professional degree (for example: MD, DDS, DVM, LLB, JD)" |
                                  respondentdata$Education == "Doctorate degree (for example: PhD, EdD)")

  # Household income
  respondentdata$Household.Income[respondentdata$Household.Income=="Prefer not to say"] <- NA
  respondentdata$hhincome_over50k <- (respondentdata$Household.Income == "$50,000-$59,999" | respondentdata$Household.Income == "$60,000-$69,999" |
                                        respondentdata$Household.Income == "$70,000-$79,999" | respondentdata$Household.Income == "$80,000-$89,999" |
                                        respondentdata$Household.Income == "$90,000-$99,999" | respondentdata$Household.Income =="$100,000-$124,999" |
                                        respondentdata$Household.Income == "$125,000-$149,999" | respondentdata$Household.Income == "$150,000-$174,999" |
                                        respondentdata$Household.Income == "$175,000-$199,999" | respondentdata$Household.Income == "$200,000-$224,999" |
                                        respondentdata$Household.Income == "$225,000-$249,999" | respondentdata$Household.Income == "$250,000 or more")

  # Age in numeric variable already
  respondentdata$age35plus <- (respondentdata$Age >= 35)

  # Gender
  respondentdata$Gender[respondentdata$Gender=="Prefer not to say"] <- NA
  respondentdata$woman <- (respondentdata$Gender == "Woman")

  # Ethnicity
  respondentdata$Ethnicity[respondentdata$Ethnicity=="Prefer not to say"] <- NA
  respondentdata$hispanic <- (respondentdata$Ethnicity != "No, not of Hispanic, Latino, or Spanish origin")

  respondentdata$respondent_na <- (base::is.na(respondentdata$black) | base::is.na(respondentdata$ba_or_more) | base::is.na(respondentdata$hhincome_over50k) |
                                   base::is.na(respondentdata$age35plus) | base::is.na(respondentdata$woman) | base::is.na(respondentdata$hispanic))

  return(respondentdata)
}
