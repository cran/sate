


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
#' @noRd
CI90 <- function(m, se)
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



#' Creates the shell of a plot used to display estimate of harm relative to harm threshold
#'
#' @return No return
#' @description Creates the shell of a plot used for compact display estimate of harm estimate
#'              relative to harm thresholds.
#' @examples
#'    library(sate)
#'    compact_harm_plot()
#' @export
#' @importFrom graphics axis mtext par plot rect
compact_harm_plot <- function()
{

  revert.par <- graphics::par(c("mar", "family", "mfrow", "mgp"))
  graphics::par(mar=base::c(2.5, 1, 1, 1), family="serif", mfrow=base::c(1,1), mgp=base::c(0,.5,.5))
  Tvalue <- .10
  graphics::plot(x="", y="", xlab="", ylab="",
       xlim=base::c(-.1, .425), ylim=base::c(0, .1), axes=F)
  graphics::axis(side=1, at=base::c(-.5,-.1,0,.1,.2,.3,.4,.5,.6),
       labels=base::c("","-.1",".0", ".1", ".2", ".3",".4",".5",""),
       line=.1, cex.axis=.7, gap.axis=0)

  graphics::rect(ybottom=-1, ytop=1, xleft=-1, xright=0, col = "white", border = F)
  graphics::rect(ybottom=-1, ytop=1, xleft=0, xright=Tvalue, col = "gray90", border = F)
  graphics::rect(ybottom=-1, ytop=1, xleft=Tvalue, xright=1, col = "gray80", border = F)
  graphics::mtext(text=base::c("No Harm", "Tolerable Harm", "Intolerable Harm"),
                  at=base::c(-.08, .05, .25), side=3, cex=.75, line=.20)
  graphics::par(revert.par)

}


#' Helper function to retrieve P(G|K) values. This should be deprecated by get_pG_by_k.
#'
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns a vector of guilty verdict probabilities for K values 0 to jury_n.
#' @description Returns a vector of P(G|K) values based on jury size. Probabilities
#'              can be generated for any jury_n (greater than 1).
#' @examples
#'    library(sate)
#'    get.model.values(12)
#'
#'    get.model.values(jury_n=8)
#' @keywords internal
#' @noRd
get.model.values <- function(jury_n)
{
  if(base::missing(jury_n)) stop("Missing jury_n value.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("jury_n must be positive integer.")
  # jury_n = 12
  P_matrix <- transition.matrix(jury_n = jury_n)
  A_matrix <- P_matrix[, c(1, jury_n+1)]
  R_matrix <- P_matrix[c(1, jury_n+1), -c(1, jury_n+1)]
  Q_matrix <- P_matrix[-c(1, jury_n+1), -c(1, jury_n+1)]
  I_matrix <- diag(jury_n - 1)
  B_matrix <- R_matrix %*% solve(I_matrix - Q_matrix)
  pG_by_k <- c(0, B_matrix[2,], 1)
  return(pG_by_k)
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
#'              statistics. This is a helper function, called by graphing functions to add layer to open graph.
#' @examples
#'    library(sate)
#'    plot(x="", y="", xlim=c(0,1), ylim=c(0, 1))
#'    plot.ellipse(pg=.70, n=400)
#'
#'    plot.ellipse(pg=.75, n=450, jury_n=6, pstrikes=3, dstrikes=3)
#' @keywords internal
#' @importFrom ellipse ellipse
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



#' Helper function to obtain expected rounds of deliberation to verdict.
#'
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns a vector of expected number of rounds of deliberation to reach a verdict for K values 0 to jury_n.
#' @description Retrieves vector of P(G|K) values based on jury size. These probabilities
#'              can be generated but accessing stored values speeds computations.
#' @examples
#'    library(sate)
#'    rounds_to_verdict(12)
#'
#'    rounds_to_verdict(jury_n=8)
#' @keywords internal
#' @noRd
rounds_to_verdict <- function(jury_n)
{
  if(base::missing(jury_n)) stop("Missing jury_n value.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("jury_n must be positive integer.")

  # rounds to verdict
  P_matrix <- transition.matrix(jury_n = jury_n)
  A_matrix <- P_matrix[, c(1, jury_n+1)]
  R_matrix <- P_matrix[c(1, jury_n+1), -c(1, jury_n+1)]
  Q_matrix <- P_matrix[-c(1, jury_n+1), -c(1, jury_n+1)]
  I_matrix <- diag(jury_n - 1)
  B_matrix <- R_matrix %*% solve(I_matrix - Q_matrix)
  ones <- rep(1, jury_n - 1)
  T_transient <- solve(I_matrix - t(Q_matrix), ones)
  T_all <- c(1, as.numeric(T_transient), 1)
  return(T_all)
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

