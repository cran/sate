#' Creates the shell of a plot showing relationship between jury pool preferences and jury verdict probabilities
#'
#' @param main Main title for plot (optional), default is "Jurors' Verdict Preferences, P(g)".
#' @param xlab X-axis label for plot (optional), default is "Jury Verdict Probabilities, P(G)".
#' @param ylab Main title for plot (optional), default is no main title.
#' @return No return
#' @description Creates the shell of a plot showing relationship between jury pool preferences and jury verdict probabilities,
#'              optional argument to modify main, xlab, and ylab labels, includes grid lines.
#' @examples
#'    library(sate)
#'    basic.plot.grid()
#'
#'    basic.plot.grid(main="Death Sentencing Analysis", xlab="Jurors' Sentencing Preferences, P(d)",
#'                    ylab="Jury Verdict Probabilities, P(D)")
#' @export
#' @importFrom graphics abline axis box mtext plot
basic.plot.grid <- function(main, xlab, ylab)
{
  if(base::missing(xlab)) xlab <- "Jurors' Verdict Preferences, P(g)"
  if(base::missing(ylab)) ylab <- "Jury Verdict Probabilities, P(G)"
  if(base::missing(main)) main <- ""
  pg_values_scale <- base::seq(0,1,by=.01)
  pG_values_scale <- sate::as.jury.point(pg_values_scale, jury_n=12, pstrikes=0, dstrikes=0, accuracy=.15)
  base::data.frame(pg_values_scale, pG_values_scale)
  graphics::plot(x="", y="", xlim=base::c(0,1), ylim=base::c(0,1),
                 xlab="", ylab="", axes=F, main=main, asp=1)
  graphics::abline(v=base::seq(0,1,by=.1), col="gray90")
  graphics::abline(h=base::seq(0,1,by=.1), col="gray90")
  graphics::axis(side=2, at=base::seq(-.1,1.0,by=.1),
                 labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
                 las=2, cex.axis=.8, line=0, hadj=.5)
  graphics::axis(side=1, at=base::seq(-.1,1.0,by=.1),
                 labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
                 cex.axis=.8, line=0, hadj=.5, padj=-1)
  graphics::mtext(text=xlab, side=1, line=2, cex=1)
  graphics::mtext(text=ylab, side=2, line=1.5, cex=1)
  graphics::box()
}
