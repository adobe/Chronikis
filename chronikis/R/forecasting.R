# Copyright 2019 Adobe. All rights reserved.
# This file is licensed to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR REPRESENTATIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

#' Draw a sample from the predictive distribution for the next \code{nsteps}
#' time steps.
#'
#' @export
#' @param models List of SSMs corresponding to posterior draws of parameters.
#' @param nsteps Number of steps ahead to forecast.
#' @param ndraws_per_model Number of draws from predictive distribution for
#'   each SSM in \code{models}.
#' @return \code{nsteps} by \code{nd} matrix of predictive draws,
#'   \code{nd} = length(\code{models}) * \code{ndraws_per_model}.
#'
forecast_sample <- function(models, nsteps, ndraws_per_model) {
  ndpm <- ndraws_per_model
  draws0 <- lapply(models, function(mdl) {
    res <- dlm::dlmForecast(mdl, nAhead = nsteps, sampleNew = ndpm)
    sapply(res$newObs, function(x){x[,1]})
  })
  nd <- ndpm * length(models)
  draws <- matrix(unlist(draws0), nr = nsteps, nc = nd)
}

#' Compute predictive mean and quantiles for future time steps.
#'
#' @export
#' @param models List of SSMs corresponding to posterior draws of parameters.
#' @param nsteps Number of steps ahead to forecast.
#' @param alpha Nonempty vector of values lying strictly between 0 and 0.5.
#' @param drop Whether to return \code{lower} and \code{upper} as vectors
#'   instead of matrices when \code{alpha} is scalar.
#' @return List with elements
#'   \itemize{
#'   \item \code{mean}: vector of predictive means, of length \code{nsteps}.
#'   \item \code{lower}: \code{nsteps} by \code{length(alpha)} matrix;
#'         \code{lower}[t,i] is quantile \code{alphas}[i]/2 of predictive
#'         distribution for time t.
#'   \item \code{upper}: \code{nsteps} by \code{length(alpha)} matrix;
#'         \code{upper}[t,i] is quantile 1-\code{alphas}[i]/2 of predictive
#'         distribution for time t.
#'  }
forecast_intervals <- function(models, nsteps, alpha, drop=FALSE) {
  ndraws <- length(models)
  means <- matrix(NA, nr=nsteps, nc=ndraws)
  stddevs <- matrix(NA, nr=nsteps, nc=ndraws)
  for (k in 1:ndraws) {
    res <- dlm::dlmForecast(models[[k]], nAhead = nsteps, sampleNew = FALSE)
    means[ , k] <- res$f[ , 1]
    stddevs[ , k] <- sqrt(unlist(res$Q))
  }
  average_normals(means, stddevs, alpha, drop)
}

#' Compute mean, upper quantiles, and lower quantiles of average of normal
#' distributions for each time step.
#'
#' @export
#' @param means An \code{nsteps} by \code{ndraws} matrix of means.
#' @param stddevs An \code{nsteps} by \code{ndraws} matrix of std devs.
#' @param alpha A nonempty vector of values lying strictly between 0 and 0.5.
#' @param drop Whether to return \code{lower} and \code{upper} as vectors
#'   instead of matrices when \code{alpha} is scalar.
#' @return List with elements
#'   \itemize{
#'   \item \code{mean}: vector of length \code{nsteps}.
#'   \item \code{lower}: \code{nsteps} by \code{nalpha} matrix.
#'   \item \code{upper}: \code{nsteps} by \code{nalpha} matrix.
#'  }
#'  where \code{mean}[t] is mean of the average of the normal distributions
#'  for step t, \code{lower}[t,i] is quantile \code{alpha}[i]/2, and
#'  \code{upper}[t,i] is quantile 1-\code{alpha}[i]/2.
#'
average_normals <- function(means, stddevs, alpha, drop=FALSE) {
  stopifnot(all(0 < alpha & alpha < 0.5))
  lb <- sapply(alpha, function(a){normavg_quantile(a/2, means, stddevs)})
  ub <- sapply(alpha, function(a){normavg_quantile(1-a/2, means, stddevs)})
  mid <- rowMeans(means)
  if (drop && length(alpha) == 1) {
    lb <- lb[, 1]
    ub <- ub[, 1]
  }
  list(mean = mid, lower=lb, upper=ub)
}

# p: desired quantile in range (0,1)
# means: nsteps x ndraws matrix of predictive means
# stddevs: nsteps x ndraws matrix of predictive stddevs
# RETURNS: vector of length nsteps
normavg_quantile <- function(p, means, stddevs) {
  stopifnot(ncol(means) > 0)
  stopifnot(nrow(means) > 0)
  stopifnot(ncol(means) == ncol(stddevs))
  stopifnot(nrow(means) == nrow(stddevs))
  stopifnot(0 < p && p < 1)
  if (ncol(means) == 1)
    return(qnorm(p, means[,1], stddevs[,1]))

  # If q[i] = max(k:: qnorm(p, means[i,k], stddevs[i,k])), then
  # pnorm(q[i], means[i,k], stddevs[i,k]) >= p for all k, hence
  # average(k:: pnorm(q[i], means[i,k], stddevs[i,k])) >= p
  ub <- matrixStats::rowMaxs(qnorm(0.01 + 0.99 * p , means, stddevs))
  pub <- rowMeans(pnorm(ub, means, stddevs))
  stopifnot(all(pub >= p))

  # If q[i] = min(k:: qnorm(1-p, means[i,k], stddevs[i,k], FALSE)), then
  # pnorm(q[i], means[i,k], stddevs[i,k], FALSE) >= 1-p for all k, hence
  # 1 - pnorm(q[i], means[i,k], stddevs[i,k]) >= 1 - p for all k, hence
  # pnorm(q[i], means[i,k], stddevs[i, k]) <= p for all k, hence
  # average(k:: pnorm(q[i], means[i,k], stddevs[i,k], FALSE)) <= p.
  lb <- matrixStats::rowMins(qnorm(0.01 + 0.99 * (1 - p), means, stddevs, lower.tail=FALSE))
  plb <- rowMeans(pnorm(lb, means, stddevs))
  stopifnot(all(plb <= p))

  # Element-wise bisection search
  change <- TRUE
  while (any(ub != lb) && change) {
    mid <- 0.5 * (ub + lb)
    pmid <- rowMeans(pnorm(mid, means, stddevs))
    lb1 <- ifelse(pmid <= p, mid, lb)
    ub1 <- ifelse(pmid >= p, mid, ub)
    change <- any(lb != lb1) || any(ub != ub1)
    ub <- ub1
    lb <- lb1
  }
  return(0.5 * (ub + lb))
}
