#' Calculates probability jury finds defendant guilty based on juror preferences
#'
#' @param pg The proportion of jurors who favor a guilty verdict.
#' @return Returns the probability jury finds defendant guilty.
#' @description Calculates probability jury finds defendant guilty based on juror preferences. Does not estimate uncertainty.
#' @examples
#'    as.jury.point(pg=.50)
#'
#'    as.jury.point(pg=10/12)
#' @export


as.jury.point <- function(pg) {
  jury_pg =  exp(-6.870279 + 11.78409*pg) / (1 + exp(-6.870279 + 11.78409*pg))
  return(jury_pg)
}
