% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecasting.R
\name{forecast_intervals}
\alias{forecast_intervals}
\title{Compute predictive mean and quantiles for future time steps.}
\usage{
forecast_intervals(models, nsteps, alpha, drop = FALSE)
}
\arguments{
\item{models}{List of SSMs corresponding to posterior draws of parameters.}

\item{nsteps}{Number of steps ahead to forecast.}

\item{alpha}{Nonempty vector of values lying strictly between 0 and 0.5.}

\item{drop}{Whether to return \code{lower} and \code{upper} as vectors
instead of matrices when \code{alpha} is scalar.}
}
\value{
List with elements
  \itemize{
  \item \code{mean}: vector of predictive means, of length \code{nsteps}.
  \item \code{lower}: \code{nsteps} by \code{length(alpha)} matrix;
        \code{lower}[t,i] is quantile \code{alphas}[i]/2 of predictive
        distribution for time t.
  \item \code{upper}: \code{nsteps} by \code{length(alpha)} matrix;
        \code{upper}[t,i] is quantile 1-\code{alphas}[i]/2 of predictive
        distribution for time t.
 }
}
\description{
Compute predictive mean and quantiles for future time steps.
}
