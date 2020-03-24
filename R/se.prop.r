#' Calculates the standard error of proportion.
#'
#' @param p The proportion (of jurors who favor a guilty verdict).
#' @param n The size of the sample used to estimate p.
#' @return Returns the standard error of a sample proportion.
#' @description Calculates the standard error of proportion.
#' @examples
#'    se.prop(p=.50, n=500)
#'
#'    se.prop(p=10/12, n=400)
#' @export

se.prop <- function(p, n) {
  se = sqrt(p*(1-p)/n)
  return(se)
}
