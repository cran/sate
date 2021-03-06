% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.jury.stats.r
\name{compare.jury.stats}
\alias{compare.jury.stats}
\title{Estimates jury-level differences based on juror-level statistics}
\usage{
compare.jury.stats(
  pg_actual,
  n_actual,
  pg_hypo,
  n_hypo,
  seed = NULL,
  ndraw = 1e+06
)
}
\arguments{
\item{pg_actual}{The proportion of jurors who favor a guilty verdict in the actual trial condition (the trial with error).}

\item{n_actual}{The size of the sample used to estimate pg_actual.}

\item{pg_hypo}{The proportion of jurors who favor a guilty verdict in the hypothetical trial condition (the fair trial without error).}

\item{n_hypo}{The size of the sample used to estimate pg_hypo.}

\item{seed}{Set seed for random number generation for replication, default is NULL (optional).}

\item{ndraw}{The number of simulations used to generate results. Should be very large number (default = 1000000).}
}
\value{
Returns a list of jury-level statistics to assess effect of a trial error.
}
\description{
Calculates jury-level differences based on juror-level statistics supplied by user.
}
\examples{
   compare.jury.stats(pg_actual=.70, n_actual=400, pg_hypo=.60, n_hypo=450)

   compare.jury.stats(pg_actual=.75, n_actual=450, pg_hypo=.65, n_hypo=350,
                      seed=12345, ndraw=100000)
}
