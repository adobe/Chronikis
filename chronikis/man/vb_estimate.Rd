% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{vb_estimate}
\alias{vb_estimate}
\title{Estimate a Chronikis model using Variational Bayes.}
\usage{
vb_estimate(y, mdl, ...)
}
\arguments{
\item{y}{A single time series, either as a ts object or a numeric vector.}

\item{mdl}{The model with arguments included, as returned by \code{setArgs} or \code{setArgsList}.}

\item{...}{Any additional arguments to pass to \code{rstan::vb}.}
}
\value{
The \code{stanfit} object containing the results of estimation.
}
\description{
Estimate a Chronikis model using Variational Bayes.
}
