#' Plots probability of a guilty verdict with confidence interval based on juror-level statistics
#'
#' @param sample_pg The proportion of jurors who favor a guilty verdict in the sample condition
#' @param sample_n The size of the sample used to estimate sample_pg_actual
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 12.
#' @param pstrikes Number of peremptory strikes by prosecution; default value is 0.
#' @param dstrikes Number of peremptory strikes by defendant; default value is 0.
#' @param accuracy Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.
#' @return No return (creates plot)
#' @description Plots probability of guilty verdict with confidence interval based on juror-level
#'              statistics supplied by user. Similar to graph.effect.defendant, but plots one condition.
#' @examples
#'    library(sate)
#'    graph.estimate(sample_pg=.70, sample_n=400)
#'
#'    graph.estimate(sample_pg=.75, sample_n=450, jury_n=6, pstrikes=3, dstrikes=3)
#' @importFrom graphics abline axis legend lines mtext par plot
#' @export
graph.estimate <- function(sample_pg, sample_n, jury_n=12, pstrikes=0, dstrikes=0, accuracy=.15)
{
  if(base::missing(sample_pg)) stop("Missing sample_pg value.")
  if(!base::is.numeric(sample_pg) || (sample_pg < 0) || (sample_pg > 1)) stop("sample_pg must be number between 0 and 1.")
  if(base::missing(sample_n)) stop("Missing sample_n value.")
  if(!base::is.numeric(sample_n) || (sample_n <= 0)) stop("sample_n must be positive number.")

  graphics::par(mar=c(4,4,0,2), mfrow=c(1,1), omi=base::c(0.1,0.1,0.1,0.1), family="serif")

  base::plot(x="", y="", xlim=base::c(0,1), ylim=base::c(0,1), xlab="", ylab="", axes=F,
       main="", asp=1)
  graphics::abline(v=base::seq(0,1,by=.1), col="gray90")
  graphics::abline(h=base::seq(0,1,by=.1), col="gray90")
  pg_values_scale <- base::seq(0,1,by=.01)
  pG_values_scale <- as.jury.point(pg_values_scale, jury_n=jury_n, pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  graphics::lines(x=pg_values_scale, y=pG_values_scale, col="gray40", lwd=1, lty=1)
  graphics::axis(side=2, at=base::seq(-.1,1.0,by=.1),
       labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
       las=2, cex.axis=.9, line=0, hadj=.5)
  graphics::axis(side=1, at=base::seq(-.1,1.0,by=.1),
       labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
       cex.axis=.9, line=0, hadj=.5, padj=0)

  # plot.ellipse is a helper function
  plot.ellipse(pg=sample_pg, n=sample_n, jury_n=jury_n, point.col="gray25", pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy)
  graphics::mtext(text="Jurors' Verdict Preferences, P(g)", side=1, line=2.5, cex=.9)
  graphics::mtext(text="Jury Verdict Probabilities, P(G)", side=2, line=2, cex=.9)

  graphics::legend(x=.50, y=.15, legend=base::c("Point Estimate", "Confidence Interval"),
         col=base::c("gray25", "gray25"),
         pch=base::c(16, -1), lty=base::c(-1, 3), cex=1, pt.cex=base::c(1.2, 1.2, 1.2, -1),
         box.col="#FFFFFF60", bg="#FFFFFF60")
}

