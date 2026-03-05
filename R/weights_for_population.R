#' Calculates survey weights given respondent information and target population demographics
#'
#' @param respondentdata Data frame with encoded respondent demographic information
#'   (use encode.cloud.respondent.variables to prepare). Must include:
#'   ParticipantId, black, ba_or_more, hhincome_over50k, age35plus, woman, hispanic, respondent_na.
#' @param targetdata A one-row data.frame (or named vector) with the following statistics
#'   in [0, 1]: black, ba_or_more, hhincome_over50k, age35plus, woman, hispanic
#'   (use target.population.demographics to obtain).
#' @return Returns respondentdata with raked sampling weights encoded.
#' @description Calculates survey weights given respondent information and target population demographics. Respondent
#'              demographic info must be properly encoded in respondentdata to work with targetdata. If
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
  assert_required(respondentdata, targetdata)
  if (!is.data.frame(respondentdata)) {
    stop("respondentdata must be a data.frame.", call. = FALSE)
  }

  required_respondent_cols <- c(
    "ParticipantId", "black", "ba_or_more", "hhincome_over50k",
    "age35plus", "woman", "hispanic", "respondent_na"
  )
  missing_respondent_cols <- setdiff(required_respondent_cols, names(respondentdata))
  if (length(missing_respondent_cols) > 0) {
    stop(
      "respondentdata is missing required column(s): ",
      paste(missing_respondent_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_target_cols <- c("black", "ba_or_more", "hhincome_over50k", "age35plus", "woman", "hispanic")
  target_vec <- if (is.data.frame(targetdata)) {
    if (nrow(targetdata) < 1) stop("targetdata must have at least one row.", call. = FALSE)
    targetdata[1, , drop = FALSE]
  } else {
    targetdata
  }
  target_names <- names(target_vec)
  if (is.null(target_names)) target_names <- character(0)
  missing_target_cols <- setdiff(required_target_cols, target_names)
  if (length(missing_target_cols) > 0) {
    stop(
      "targetdata is missing required field(s): ",
      paste(missing_target_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  to_logical01 <- function(x, name) {
    if (is.logical(x)) return(x)
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- toupper(trimws(x))
      ok <- is.na(x) | x %in% c("TRUE", "FALSE")
      if (!all(ok)) stop(name, " must contain TRUE/FALSE values (or NA).", call. = FALSE)
      return(ifelse(is.na(x), NA, x == "TRUE"))
    }
    if (is.numeric(x)) {
      ok <- is.na(x) | x %in% c(0, 1)
      if (!all(ok)) stop(name, " must contain only 0/1 values (or NA).", call. = FALSE)
      return(ifelse(is.na(x), NA, x == 1))
    }
    stop(name, " must be logical, character TRUE/FALSE, or numeric 0/1.", call. = FALSE)
  }

  respondentdata <- respondentdata
  respondentdata$black <- to_logical01(respondentdata$black, "respondentdata$black")
  respondentdata$ba_or_more <- to_logical01(respondentdata$ba_or_more, "respondentdata$ba_or_more")
  respondentdata$hhincome_over50k <- to_logical01(respondentdata$hhincome_over50k, "respondentdata$hhincome_over50k")
  respondentdata$age35plus <- to_logical01(respondentdata$age35plus, "respondentdata$age35plus")
  respondentdata$woman <- to_logical01(respondentdata$woman, "respondentdata$woman")
  respondentdata$hispanic <- to_logical01(respondentdata$hispanic, "respondentdata$hispanic")
  respondentdata$respondent_na <- to_logical01(respondentdata$respondent_na, "respondentdata$respondent_na")

  if (any(is.na(respondentdata$ParticipantId))) {
    stop("respondentdata$ParticipantId contains missing values.", call. = FALSE)
  }
  if (anyDuplicated(respondentdata$ParticipantId) > 0) {
    stop("respondentdata$ParticipantId must contain unique values.", call. = FALSE)
  }

  target_vals <- unlist(target_vec[required_target_cols], use.names = TRUE)
  if (!is.numeric(target_vals) || any(is.na(target_vals)) || any(!is.finite(target_vals))) {
    stop("targetdata demographic fields must be finite numeric values.", call. = FALSE)
  }
  if (any(target_vals < 0 | target_vals > 1)) {
    stop("targetdata demographic fields must be between 0 and 1.", call. = FALSE)
  }

  # for debugging purposes: respondentdata <- surveydata
  pop.black <- as.numeric(target_vals["black"])
  pop.ba_or_more <- as.numeric(target_vals["ba_or_more"])
  pop.hhincome_over50k <- as.numeric(target_vals["hhincome_over50k"])
  pop.age35plus <- as.numeric(target_vals["age35plus"])
  pop.woman <- as.numeric(target_vals["woman"])
  pop.hispanic <- as.numeric(target_vals["hispanic"])

  respondentdata.all <- respondentdata
  respondentdata.svy.unweighted <- survey::svydesign(ids=~1, weights=~1,
                                          data=subset(respondentdata.all, !respondentdata.all$respondent_na))
  nrows <- base::nrow(respondentdata.svy.unweighted$variables)
  if (nrows <= 0) {
    stop("No eligible rows remain after excluding respondent_na == TRUE.", call. = FALSE)
  }

  # population targets multipled by nrow(respondentdata.all)
  # gives the target distribution for the respondentdata
  target.black.dist <- base::data.frame(black = c(TRUE, FALSE), Freq = nrows * c(pop.black, 1 - pop.black))
  target.ba_or_more.dist <- base::data.frame(ba_or_more = c(TRUE, FALSE), Freq = nrows * c(pop.ba_or_more, 1 - pop.ba_or_more))
  target.hhincome_over50k.dist <- base::data.frame(hhincome_over50k = c(TRUE, FALSE), Freq = nrows * c(pop.hhincome_over50k, 1 - pop.hhincome_over50k))
  target.age35plus.dist <- base::data.frame(age35plus = c(TRUE, FALSE), Freq = nrows * c(pop.age35plus, 1 - pop.age35plus))
  target.woman.dist <- base::data.frame(woman = c(TRUE, FALSE), Freq = nrows * c(pop.woman, 1 - pop.woman))
  target.hispanic.dist <- base::data.frame(hispanic = c(TRUE, FALSE), Freq = nrows * c(pop.hispanic, 1 - pop.hispanic))

  respondentdata.svy.rake <- survey::rake(design = respondentdata.svy.unweighted,
                                  sample.margins = base::list(~black, ~ba_or_more, ~hhincome_over50k, ~age35plus, ~woman, ~hispanic),
                                  population.margins = base::list(target.black.dist, target.ba_or_more.dist, target.hhincome_over50k.dist,
                                                            target.age35plus.dist, target.woman.dist, target.hispanic.dist))

  respondentdata.svy.rake <- survey::trimWeights(respondentdata.svy.rake, upper = 6, lower = .1)
  # there seem to be different rules of thumb but it seems to be a good quality control

  if(sum(respondentdata.all$respondent_na, na.rm = TRUE) > 0) {
    # the rake function can't handle missing data, so need to store separately and then merge
    weights_data <- base::data.frame(weights = stats::weights(respondentdata.svy.rake),
                                     ParticipantId = respondentdata.svy.rake$variables$ParticipantId)
    ##### check this part, it may need to come later or be broken into parts
    merged_respondentdata <- base::merge(respondentdata.all, weights_data, by = base::c("ParticipantId"), all.x=TRUE)
    respondentdata.all <- merged_respondentdata # should add one variable (weights) to respondentdata
  }
  else {
    respondentdata.all$weights <- stats::weights(respondentdata.svy.rake)
  }

  # if weight not calculated (respondent info missing), give respondent mean weight
  respondentdata.all$weights[base::is.na(respondentdata.all$weights)] <- base::mean(respondentdata.all$weights, na.rm=T)
  # make sure mean weight = 1
  respondentdata.all$weights <- respondentdata.all$weights / base::mean(respondentdata.all$weights, na.rm=T)
  return(respondentdata.all)
  # continue debugging: surveydata <- respondentdata.all
}

