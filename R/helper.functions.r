
#' Helper function to retrieve P(G|K) values.
#'
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns a vector of guilty verdict probabilities for K values 0 to jury_n.
#' @description Retrieves vector of P(G|K) values based on jury size. These probabilities
#'              can be generated but accessing stored values speeds computations.
#' @examples
#'    library(sate)
#'    as.jury.stats(sample_pg=.50, sample_n=830)
#'
#'    as.jury.stats(sample_pg=10/12, sample_n=295)
#' @keywords internal
# @importFrom base c is.null is.numeric message missing
#' @noRd
get.model.values <- function(jury_n)
{
  if(base::missing(jury_n)) stop("Missing jury_n value.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("jury_n must be positive integer.")

  prob_G_by_ng <- NULL
  if(jury_n==12) prob_G_by_ng <- base::c(0.0000, 0.0001, 0.0014, 0.0086, 0.0377, 0.1191,
                                         0.2779, 0.5010, 0.7232, 0.8825, 0.9638, 0.9927, 1.0000)
  else if(jury_n==6) prob_G_by_ng <- base::c(0.0000, 0.0044, 0.0448, 0.2064, 0.5217, 0.8380, 1.0000)
  else if(jury_n==8) prob_G_by_ng <- base::c(0, 0.001352, 0.013442, 0.072006, 0.236054,
                                                0.506787, 0.777377, 0.941337, 1.000000)
  else if(jury_n==4) prob_G_by_ng <- base::c(0, 0.016832, 0.166799, 0.582394, 1.000000)
  else if(jury_n==16) prob_G_by_ng <- base::c(0, 0.000012, 0.000119, 0.000994, 0.005312,
                                                 0.020486, 0.063226, 0.154504, 0.307323,
                                                 0.500089, 0.693865, 0.845554, 0.936905,
                                                 0.979330, 0.994850, 0.999129, 1.000000)

  if(base::is.null(prob_G_by_ng)) base::message("Sorry, do not have P(G|k) values for that jury size.")
  else return(prob_G_by_ng)
}




#' Helper function to Calculate the standard error of proportion.
#'
#' @param p The proportion (e.g. proportion of jurors who favor a guilty verdict).
#' @param n The size of the sample used to estimate p.
#' @return Returns the standard error of a sample proportion.
#' @description Calculates the standard error of proportion.
#' @examples
#'    library(sate)
#'    se.prop(p=.50, n=500)
#'
#'    se.prop(p=10/12, n=400)
#' @keywords internal
# @importFrom base is.numeric missing sqrt
#' @noRd
se.prop <- function(p, n)
{
  if(base::missing(p)) stop("Missing p value.")
  if(!base::is.numeric(p) || (p < 0) || (p > 1)) stop("p must be number between 0 and 1.")
  if(base::missing(n)) stop("Missing n value.")
  if(!base::is.numeric(n) || (n <= 0)) stop("Initial n must be positive number.")

  se = base::sqrt(p*(1-p)/n)
  # print(se)
  return(se)
}


#' Helper function to calculate the 90 percent confidence interval of a proportion.
#'
#' @param p The sample proportion (e.g. proportion of of jurors who favor a guilty verdict).
#' @param se The standard error of the sample proportion, p.
#' @return Returns the 90 percent confidence interval as a list.
#' @description Calculates the 90 percent confidence interval of a proportion. 90 percent confidence interval used to test one-sided hypothesis at .05 level.
#' @examples
#'    library(sate)
#'    CI90(m=.5, se=.04)
#'
#'    CI90(m=10/12, se=.02)
#' @keywords internal
#' @importFrom stats qnorm
# @importFrom base c is.numeric missing names
#' @noRd
CI90 <- function (m, se)
{
  if(base::missing(m)) stop("Missing m value.")
  if(!base::is.numeric(m)) stop("m must be a number.")
  if(base::missing(se)) stop("Missing se value.")
  if(!base::is.numeric(se) || (se < 0)) stop("se must be non-negative number.")

  lower = m - stats::qnorm(0.95) * se
  upper = m + stats::qnorm(0.95) * se
  out1 = base::c(lower, upper)
  # print(out1)
  base::names(out1) = base::c("Lower 90% CI", "     Upper 90% CI")
  return(out1)
}





#' Helper function use to plot point estimates with confidence intervals
#'
#' @param pg The proportion of jurors who favor a guilty verdict
#' @param n The size of the sample used to estimate pg
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param point.col The color of point at center of ellipse; default is "gray25"
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @return No return (adds element to plot)
#' @description Plots probability of guilty verdict with confidence interval based on juror-level
#'              statistics.
#' @examples
#'    library(sate)
#'    plot(x="", y="", xlim=c(0,1), ylim=c(0, 1))
#'    plot.ellipse(pg=.70, n=400)
#'
#'    plot.ellipse(pg=.75, n=450, jury_n=6, pstrikes=3, dstrikes=3)
#' @keywords internal
#' @importFrom ellipse ellipse
# @importFrom base c is.numeric matrix missing sqrt
#' @importFrom graphics lines points polygon
#' @noRd
plot.ellipse <- function(pg, n, jury_n=12, point.col="gray25", pstrikes=0, dstrikes=0, accuracy=.15)
{
  if(base::missing(pg)) stop("Missing pg value.")
  if(!base::is.numeric(pg) || (pg < 0) || (pg > 1)) stop("pg must be number between 0 and 1.")
  if(base::missing(n)) stop("Missing n value.")
  if(!base::is.numeric(n) || (n <= 0)) stop("n must be positive number.")

  # general form
  ci_color <- "#BCBCBC70"
  jurors.se  <- se.prop(p=pg, n=n)
  jury.stats <- as.jury.stats(sample_pg=pg, sample_n=n, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  # the +/- .01 here is used for slope of line at point
  A <- (as.jury.point(sample_pg=pg +.01, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy) - as.jury.point(sample_pg=pg -.01, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)) / .02
  H <- base::sqrt(1 + A^2)
  rotation_matrix <- base::matrix(nrow=2, byrow=T, data=base::c(A/H, -1/H, 1/H, A/H))
  ellipse <- ellipse::ellipse(x=0, centre=c(0, 0),
                              scale=c(jurors.se, jury.stats$SE),
                              level=.90) %*% rotation_matrix
  ellipse[,1] <- ellipse[,1] + pg
  ellipse[,2] <- ellipse[,2] + jury.stats$PG
  graphics::polygon(ellipse[,1], ellipse[,2], col=ci_color, border=F)
  graphics::lines(ellipse, col="gray25", lty=3)
  graphics::points(x=pg, y=jury.stats$PG, col=point.col, pch=16)
  graphics::points(x=pg, y=jury.stats$PG, col="black", pch=1)
}

