#' Calculates the 90 percent confidence interval of a proportion.
#'
#' @param p The sample proportion (of jurors who favor a guilty verdict).
#' @param se The standard error of the sample proportion, p.
#' @return Returns the 90 percent confidence interval as a list.
#' @description Calculates the 90 percent confidence interval of a proportion. 90 percent confidence interval used to test one-sided hypothesis at .05 level.
#' @examples
#'    CI90(p=.5, se=.04)
#'
#'    CI90(p=10/12, se=.02)
#' @export


CI90 <- function(p, se) {
  lower = p - stats::qnorm(0.95) * se
  upper = p + stats::qnorm(0.95) * se
  out1 = c(lower, upper)
  names(out1) = c("Lower 90% CI", "     Upper 90% CI")
  return(out1)
}
