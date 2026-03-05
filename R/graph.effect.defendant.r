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
#' @param simulate Simulation control passed to `compare.jury.stats` and `as.jury.stats`.
#'   Use `FALSE` (default), `TRUE`, or a named list like `list(nDraws=5000, seed=12345)`.
#'   For graphing speed, `TRUE` uses `nDraws=2000` by default.
#' @return No return (creates plots)
#' @description Plots jury-level differences based on juror-level statistics supplied by user. Point
#'              estimates supplemented by confidence intervals. Effect-on-defendant also plotted.
#' @examples
#'    library(sate)
#'    graph.effect.defendant(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)
#'
#'    graph.effect.defendant(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
#'                          jury_n=6, pstrikes=3, dstrikes=3)
#'
#'    graph.effect.defendant(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450, simulate=TRUE)
#' @importFrom graphics axis layout legend lines mtext par points plot rect segments
#' @export
graph.effect.defendant <- function(pg_actual, n_actual, pg_hypo, n_hypo, jury_n=12,
                                   pstrikes=0, dstrikes=0, accuracy=.15, simulate=FALSE)
{
  assert_required(pg_actual, n_actual, pg_hypo, n_hypo)
  assert_between_0_1(pg_actual, pg_hypo, accuracy)
  assert_positive_numeric(n_actual, n_hypo)
  assert_positive_integer(jury_n)
  assert_nonnegative_integer(pstrikes, dstrikes)

  # Graphing uses fewer simulation draws by default for speed.
  simulate_for_graph <- simulate
  if (isTRUE(simulate_for_graph)) {
    simulate_for_graph <- list(nDraws = 2000L, seed = NULL)
  } else if (is.list(simulate_for_graph) && !("nDraws" %in% names(simulate_for_graph))) {
    simulate_for_graph$nDraws <- 2000L
  }

  graphics::layout(matrix(c(1,1,1,2), nrow = 1, ncol = 4, byrow = TRUE))
  graphics::par(mar=c(4,4,0,2), omi=base::c(0.1,0.1,0.1,0.1), family="serif")

  graphics::plot(x="", y="", xlim=base::c(0,1), ylim=base::c(0,1), xlab="", ylab="", axes=F, main="", asp=1)
  graphics::abline(v=base::seq(0,1,by=.1), col="gray90")
  graphics::abline(h=base::seq(0,1,by=.1), col="gray90")
  # the juror to jury line depends only on jury size (it is not estimated from samples)
  # the line needs to depend on strikes too
  pg_values_scale <- base::seq(0,1,by=.01)
  pG_values_scale <- as.jury.point(pg_values_scale, jury_n=jury_n, pstrikes=pstrikes,
                                   dstrikes=dstrikes, accuracy=accuracy, simulate=simulate_for_graph)
  graphics::lines(x=pg_values_scale, y=pG_values_scale, col="gray40", lwd=1, lty=1)
  graphics::axis(side=2, at=base::seq(-.1,1.0,by=.1),
       labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
       las=2, cex.axis=.9, line=0, hadj=.5)
  graphics::axis(side=1, at=base::seq(-.1,1.0,by=.1),
       labels=base::c("","0",".1",".2",".3",".4",".5",".6",".7",".8",".9","1"),
       cex.axis=.9, line=0, hadj=.5, padj=0)

  # actual trial
  plot.ellipse(pg=pg_actual, n=n_actual, jury_n=jury_n, point.col="black",
               pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy, simulate=simulate_for_graph)
  # hypothetical trial
  plot.ellipse(pg=pg_hypo, n=n_hypo, jury_n=jury_n, point.col="white",
               pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy, simulate=simulate_for_graph)

  graphics::mtext(text="Jurors' Verdict Preferences, P(g)", side=1, line=2.5, cex=.9)
  graphics::mtext(text="Jury Verdict Probabilities, P(G)", side=2, line=2, cex=.9)

  graphics::legend(x=.50, y=.25, legend=base::c("Actual trial", "Hypothetical trial",
                                               "Trial error effect", "Confidence interval"),
         col=base::c("black", "black", "black", "gray25"),
         pch=base::c(16, 1, 7, -1), lty=c(-1, -1, -1, 3), cex=1, pt.cex=base::c(1.2, 1.2, 1.2, -1),
         box.col="#FFFFFF60", bg="#FFFFFF60")
  graphics::box()
  # right hand side plot of effect size



  effect.stats <- compare.jury.stats(pg_actual=pg_actual, n_actual=n_actual,
                                     pg_hypo=pg_hypo, n_hypo=n_hypo, jury_n=jury_n,
                                     pstrikes=pstrikes, dstrikes=dstrikes, accuracy=accuracy,
                                     simulate=simulate_for_graph)$difference
  effect_value <- if ("Difference in P(G)" %in% names(effect.stats)) {
    effect.stats[["Difference in P(G)"]]
  } else if ("PG" %in% names(effect.stats)) {
    effect.stats[["PG"]]
  } else {
    stop("effect.stats must contain 'Difference in P(G)' or 'PG'.", call. = FALSE)
  }
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
  graphics::segments(y0=effect.stats$`Lower 5%`,  x0 = horizontal_placement,
                     y1=effect.stats$`Upper 95%`, x1 = horizontal_placement, lty=3, col="black")

  # poly_scale = .02
  # graphics::polygon(x= horizontal_placement + poly_scale*c(cos(pi/4), cos(pi), cos(7*pi/4)),
  #                   y= effect_value + poly_scale*c(sin(pi/4), sin(pi), sin(7*pi/4)),
  #                   border = "black", col="gray20") #

  graphics::points(y=effect_value, x=horizontal_placement,
                   pch=15, col="white", cex=1.5) #
  graphics::points(y=effect_value, x=horizontal_placement,
                   pch=7, col="black", cex=1.5) #
  graphics::mtext(text=base::c("No\nHarm", "Tolerable\nHarm", "Intolerable\nHarm"), at=base::c(-.11, .05, .30),
        side=2, cex=.8, line=3.5, las=2, adj=.5)
}
