#' Plots jury-level differences based on juror-level statistics with effect-on-defendant displayed
#'
#' @param pg_actual The proportion of jurors who favor a guilty verdict in the actual trial condition (the trial with error).
#' @param n_actual The size of the sample used to estimate pg_actual.
#' @param pg_hypo The proportion of jurors who favor a guilty verdict in the hypothetical trial condition (the fair trial without error).
#' @param n_hypo The size of the sample used to estimate pg_hypo.
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @return No return (creates plots)
#' @description Plots jury-level differences based on juror-level statistics supplied by user. Point
#'              estimates supplemented by confidence intervals. Effect-on-defendant also plotted.
#' @examples
#'    library(sate)
#'    graph.effect.defendant(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)
#'
#'    graph.effect.defendant(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
#'                          jury_n=6, pstrikes=3, dstrikes=3)
# @importFrom base c is.numeric missing seq
#' @importFrom graphics axis layout legend lines mtext par points plot rect segments
#' @export
graph.effect.defendant <- function(pg_actual, n_actual, pg_hypo, n_hypo, jury_n=12,
                                  pstrikes=0, dstrikes=0, accuracy=.15)
{
  if(base::missing(pg_actual)) stop("Missing pg_actual value.")
  if(!base::is.numeric(pg_actual) || (pg_actual < 0) || (pg_actual > 1)) stop("pg_actual must be number between 0 and 1.")
  if(base::missing(pg_hypo)) stop("Missing pg_hypo value.")
  if(!base::is.numeric(pg_hypo) || (pg_hypo < 0) || (pg_hypo > 1)) stop("pg_hypo must be number between 0 and 1.")
  if(base::missing(n_actual)) stop("Missing n_actual value.")
  if(!base::is.numeric(n_actual) || (n_actual <= 0)) stop("n_actual must be positive number.")
  if(base::missing(n_hypo)) stop("Missing n_hypo value.")
  if(!base::is.numeric(n_hypo) || (n_hypo <= 0)) stop("n_hypo must be positive number.")

  graphics::layout(matrix(c(1,1,1,2), nrow = 1, ncol = 4, byrow = TRUE))
  graphics::par(mar=c(4,4,0,2), omi=base::c(0.1,0.1,0.1,0.1), family="serif")

  graphics::plot(x="", y="", xlim=base::c(0,1), ylim=base::c(0,1), xlab="", ylab="", axes=F, main="", asp=1)
  graphics::abline(v=base::seq(0,1,by=.1), col="gray90")
  graphics::abline(h=base::seq(0,1,by=.1), col="gray90")
  # the juror to jury line depends only on jury size (it is not estimated from samples)
  # the line needs to depend on strikes too
  pg_values_scale <- base::seq(0,1,by=.01)
  pG_values_scale <- as.jury.point(pg_values_scale, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  graphics::lines(x=pg_values_scale, y=pG_values_scale, col="gray40", lwd=1, lty=1)
  graphics::axis(side=2, at=base::seq(-.1,1.0,by=.1),
       labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
       las=2, cex.axis=.9, line=0, hadj=.5)
  graphics::axis(side=1, at=base::seq(-.1,1.0,by=.1),
       labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
       cex.axis=.9, line=0, hadj=.5, padj=0)

  # actual trial
  plot.ellipse(pg=pg_actual, n=n_actual, jury_n=jury_n, point.col="black", pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  # hypothetical trial
  plot.ellipse(pg=pg_hypo, n=n_hypo, jury_n=jury_n, point.col="white", pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)

  graphics::mtext(text="Jurors' Verdict Preferences, P(g)", side=1, line=2.5, cex=.9)
  graphics::mtext(text="Jury Verdict Probabilities, P(G)", side=2, line=2, cex=.9)

  graphics::legend(x=.60, y=.2, legend=base::c("Actual Trial", "Hypothetical Trial", "Trial Error Effect", "Confidence Interval"),
         col=base::c("black", "black", "black", "gray25"),
         pch=base::c(16, 1, 10, -1), lty=c(-1, -1, -1, 3), cex=1.2, pt.cex=base::c(1.2, 1.2, 1.2, -1),
         box.col="#FFFFFF60", bg="#FFFFFF60")

  effect.stats <- compare.jury.stats(pg_actual=pg_actual, n_actual=n_actual,
                                     pg_hypo=pg_hypo, n_hypo=n_hypo, jury_n=jury_n,
                                     pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  Tvalue <- .10
  graphics::par(mar=c(4, 5, 3, 1), family="serif")
  graphics::plot(x="", y="",
       xlab="",
       ylab="",
       ylim=base::c(-.2, .5), xlim=base::c(0, .1), axes=F)
  graphics::axis(side=2, at=base::c(-.5,0,.1,.2,.3,.4,.5,.6),
                 labels=base::c("",".0", ".1", ".2", ".3",".4",".5",""),
                 las=2, line=0, hadj=.5)
  graphics::rect(ybottom=-.5, xleft=0, ytop=0, xright=1, col = "gray95", border = F)
  graphics::rect(ybottom=0, xleft=0, ytop=Tvalue, xright=1, col = "gray85", border = F)
  graphics::rect(ybottom=Tvalue, xleft=0, ytop=1, xright=1, col = "gray70", border = F)

  # showing effect CI
  horizontal_placement <- .05
  graphics::segments(y0=effect.stats$CI_diff["Lower 5%"],  x0 = horizontal_placement,
           y1=effect.stats$CI_diff["Upper 95%"], x1 = horizontal_placement, lty=3, col="black")
  graphics::points(y=base::c(effect.stats$PG_diff, effect.stats$PG_diff),
         x=base::c(horizontal_placement, horizontal_placement),
         pch=base::c(16, 10), col=base::c("white","black"), cex=c(1.1,1.1)) #
  graphics::mtext(text=base::c("No\nHarm", "Tolerable\nHarm", "Intolerable\nHarm"), at=base::c(-.11, .05, .30),
        side=2, cex=.8, line=3.5, las=2, adj=.5)
}
