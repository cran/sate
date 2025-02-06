


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


#' Helper function to retrieve P(G|K) values.
#'
#' @param jury_n Size of the jury (i.e. 4, 6, 8, 12, or 16).
#' @return Returns a vector of guilty verdict probabilities for K values 0 to jury_n.
#' @description Retrieves vector of P(G|K) values based on jury size. These probabilities
#'              can be generated but accessing stored values speeds computations.
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
  # this would be better as switch statement
  # switch will match numeric values by argument position, not value
  switch(EXPR = as.character(jury_n),
          "6"={ pG_by_k = c(0, 0.00448, 0.04482, 0.20617, 0.52241, 0.83865, 1) },
         "12" = { pG_by_k = c(0, 0.00014, 0.00138, 0.00869, 0.03793, 0.11915, 0.27834, 0.50069, 0.72303, 0.88223, 0.96345, 0.99269, 1); },
          "4" = { pG_by_k = c(0, 0.01667, 0.16667, 0.58333, 1); },
          "8" = { pG_by_k = c(0, 0.00135, 0.01353, 0.07246, 0.23616, 0.50676, 0.77737, 0.94107, 1); },
         "10" = { pG_by_k = c(0, 0.00043, 0.00427, 0.02519, 0.09733, 0.25965, 0.50213, 0.74462, 0.90694, 0.97908, 1); },
         "14" = { pG_by_k = c(0, 5e-05, 0.00045, 0.00298, 0.01427, 0.05084, 0.13819, 0.29348, 0.50023, 0.70697, 0.86226, 0.94961, 0.98618, 0.99747, 1); },
         "16" = { pG_by_k = c(0, 1e-05, 0.00015, 0.00102, 0.00525, 0.02066, 0.06345, 0.15486, 0.30598, 0.50007, 0.69417, 0.84528, 0.9367, 0.97949, 0.9949, 0.99913, 1); },
         "18" = { pG_by_k = c(0, 0, 5e-05, 0.00035, 0.0019, 0.00811, 0.02754, 0.07552, 0.16955, 0.31649, 0.50002, 0.68356, 0.8305, 0.92453, 0.97251, 0.99194, 0.99815, 0.9997, 1); },
         "20" = { pG_by_k = c(0, 0, 2e-05, 0.00012, 0.00068, 0.00311, 0.01147, 0.03469, 0.08694, 0.18259, 0.32547, 0.50001, 0.67455, 0.81743, 0.91307, 0.96533, 0.98855, 0.99691, 0.99933, 0.9999, 1); },
         "22" = { pG_by_k = c(0, 0, 1e-05, 4e-05, 0.00024, 0.00117, 0.00463, 0.01522, 0.04195, 0.09771, 0.19424, 0.33325, 0.5, 0.66675, 0.80576, 0.9023, 0.95806, 0.98478, 0.99538, 0.99884, 0.99976, 0.99996, 1); },
         "24" = { pG_by_k = c(0, 0, 0, 1e-05, 9e-05, 0.00043, 0.00182, 0.00645, 0.01928, 0.0492, 0.10782, 0.20473, 0.34008, 0.5, 0.65992, 0.79527, 0.89218, 0.95081, 0.98072, 0.99356, 0.99818, 0.99957, 0.99992, 0.99999, 1); },
          "5" = { pG_by_k = c(0, 0.00844, 0.08442, 0.34648, 0.73794, 1); },
          "7" = { pG_by_k = c(0, 0.00244, 0.02443, 0.12241, 0.35643, 0.668, 0.90202, 1); },
          "9" = { pG_by_k = c(0, 0.00076, 0.00757, 0.04277, 0.15295, 0.36891, 0.63866, 0.85462, 0.9648, 1); },
         "11" = { pG_by_k = c(0, 0.00024, 0.00242, 0.0148, 0.06109, 0.17784, 0.37997, 0.62244, 0.82458, 0.94133, 0.98762, 1); },
         "13" = { pG_by_k = c(0, 8e-05, 0.00079, 0.00509, 0.02335, 0.07841, 0.19839, 0.38915, 0.61164, 0.80239, 0.92238, 0.97744, 0.99569, 1); },
         "15" = { pG_by_k = c(0, 3e-05, 0.00026, 0.00175, 0.00867, 0.03257, 0.09441, 0.21562, 0.39668, 0.60358, 0.78464, 0.90585, 0.96769, 0.99158, 0.99851, 1); },
         "17" = { pG_by_k = c(0, 1e-05, 9e-05, 6e-04, 0.00316, 0.01299, 0.04205, 0.10907, 0.23027, 0.40293, 0.59715, 0.76982, 0.89102, 0.95803, 0.9871, 0.99692, 0.99949, 1); },
         "19" = { pG_by_k = c(0, 0, 3e-05, 2e-04, 0.00114, 0.00503, 0.01785, 0.05152, 0.12246, 0.24291, 0.40819, 0.59183, 0.75712, 0.87757, 0.94851, 0.98218, 0.995, 0.99889, 0.99982, 1); },
         "21" = { pG_by_k = c(0, 0, 1e-05, 7e-05, 0.00041, 0.00191, 0.00731, 0.02309, 0.0608, 0.1347, 0.25394, 0.41269, 0.58732, 0.74607, 0.86531, 0.93921, 0.97691, 0.9927, 0.9981, 0.9996, 0.99994, 1); },
         "23" = { pG_by_k = c(0, 0, 0, 2e-05, 0.00014, 0.00071, 0.00291, 0.00994, 0.02859, 0.06981, 0.14591, 0.26368, 0.41659, 0.58341, 0.73633, 0.85409, 0.9302, 0.97141, 0.99006, 0.99709, 0.99929, 0.99986, 0.99998, 1); },
         "25" = { pG_by_k = c(0, 0, 0, 1e-05, 5e-05, 0.00026, 0.00114, 0.00415, 0.01288, 0.03424, 0.07849, 0.15622, 0.27235, 0.42002, 0.57998, 0.72765, 0.84378, 0.92152, 0.96576, 0.98712, 0.99585, 0.99886, 0.99974, 0.99995, 0.99999, 1); },
         { base::message("Sorry, do not have P(G|k) values for that jury size."); return(NULL) }
  )

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



