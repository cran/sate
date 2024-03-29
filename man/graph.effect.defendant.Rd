% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.effect.defendant.r
\name{graph.effect.defendant}
\alias{graph.effect.defendant}
\title{Plots jury-level differences based on juror-level statistics with effect-on-defendant displayed}
\usage{
graph.effect.defendant(
  pg_actual,
  n_actual,
  pg_hypo,
  n_hypo,
  jury_n = 12,
  pstrikes = 0,
  dstrikes = 0,
  accuracy = 0.15
)
}
\arguments{
\item{pg_actual}{The proportion of jurors who favor a guilty verdict in the actual trial condition (the trial with error).}

\item{n_actual}{The size of the sample used to estimate pg_actual.}

\item{pg_hypo}{The proportion of jurors who favor a guilty verdict in the hypothetical trial condition (the fair trial without error).}

\item{n_hypo}{The size of the sample used to estimate pg_hypo.}

\item{jury_n}{Size of the jury (i.e. 6, 8, or 12); default value is 12.}

\item{pstrikes}{Number of peremptory strikes by prosecution; default value is 0.}

\item{dstrikes}{Number of peremptory strikes by defendant; default value is 0.}

\item{accuracy}{Accuracy of parties' peremptory strikes; a number between 0 and 1; default value is .15.}
}
\value{
No return (creates plots)
}
\description{
Plots jury-level differences based on juror-level statistics supplied by user. Point
             estimates supplemented by confidence intervals. Effect-on-defendant also plotted.
}
\examples{
   library(sate)
   graph.effect.defendant(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)

   graph.effect.defendant(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
                         jury_n=6, pstrikes=3, dstrikes=3)
}
