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

# TODO: Is there a way to get around need to issue library(rstan)?

runSSMcreation <- function(fpathCKS, fpathStan, fpathR, fctNameR) {
  is.string <- function(x){ is.character(x) && length(x) == 1 }
  stopifnot(is.string(fpathCKS))
  stopifnot(is.string(fpathStan))
  stopifnot(is.string(fpathR))
  stopifnot(missing(fctNameR) || is.string(fctNameR))

  args <- c("--stan", fpathStan, "--R", fpathR)
  if (!missing(fctNameR))
    args <- c(args, "--rfct", fctNameR)

  res <- system2("ssm-creation-exe", args=args, stdin=fpathCKS, stdout=TRUE, stderr=TRUE)
  exit_code <- attr(res, "status")

  if (length(res) != 0) {
    for (s in res)
      cat(s, "\n")
  }
  if (!is.null(exit_code) && exit_code != 0)
    stop("Failed running ssm-creation-exe, exit code ", exit_code)
  as.character(res)
}

#' Compile a Chronikis program
#'
#' @export
#' @param fpathCKS Path to Chronikis source file.
#' @param fctNameR (Optional) Name of R SSM-creation function to generate.
#'   Defaults to basename of sourcefile with .cks extension removed, prefixed
#'   with "createSSMs_". In all cases, if the result is not a valid R name
#'   then it is converted to one in the same way as the `R` function
#'  `make.names`.
#' @return \code{stanmodel} object obtained by compiling the Chronikis program
#'   to Stan and then running the Stan compiiler.
#'
cksCompile <- function(fpathCKS, fctNameR) {
  stopifnot(fs::path_ext(fpathCKS) == "cks")
  fstem <- fs::path_ext_remove(fpathCKS)
  fpathStan <- fs::path_ext_set(fstem, "stan")
  fpathR <- fs::path_ext_set(fstem, "R")
  cat(".cks -> .stan...\n")
  outp <- runSSMcreation(fpathCKS, fpathStan, fpathR, fctNameR)
  cat(".stan -> .rds...\n")
  sm <- rstan::stan_model(file = fpathStan, auto_write = TRUE)
  sm
}

#' Create arguments to pass to a Chronikis model.
#'
#' @export
mdlArgs <- function(...) {
  args <- list(...)
  names(args) <- paste0(names(args), "_")
  args
}

#' Add model arguments to compiled model
#'
#' @export
setArgs <- function(stanmodel, args) {
  stopifnot('stanmodel' %in% class(stanmodel))
  list(stanmodel=stanmodel, args=args)
}

#' @export
setArgsList <- function(standmodel, ...) {
  setArgs(stanmodel, mdlArgs(...))
}

#' Estimate a Chronikis model using HMC.
#'
#' @export
#' @param y A single time series, either as a ts object or a numeric vector.
#' @param mdl The model with arguments included, as returned by \code{setArgs} or \code{setArgsList}.
#' @param ... Any additional arguments to pass to \code{rstan::sampling}, such as \code{cores},
#'   \code{chains}, \code{iter}, \code{warmup}, \code{thin}, or \code{control}.
#' @return The \code{stanfit} object containing the results of estimation.
#'
hmc_estimate <- function(y, mdl, ...) {
  standata <- estimation_data(y, mdl$args)
  fit <- rstan::sampling(mdl$stanmodel, data=standata, ...)
  rstan::check_hmc_diagnostics(fit)
  fit
}

#' Estimate a Chronikis model using Variational Bayes.
#'
#' @export
#' @param y A single time series, either as a ts object or a numeric vector.
#' @param mdl The model with arguments included, as returned by \code{setArgs} or \code{setArgsList}.
#' @param ... Any additional arguments to pass to \code{rstan::vb}.
#' @return The \code{stanfit} object containing the results of estimation.
#'
vb_estimate <- function(y, mdl, ...) {
  standata <- estimation_data(y, mdl$args)
  fit <- rstan::vb(mdl$stanmodel, data=standata, ...)
  if (fit@mode != 0)
    message(sprintf("Variational Bayes estimation failed with code %d.", fit@mode))
  fit
}

#' Estimate a Chronikis model using MAP.
#'
#' @export
#' @param y A single time series, either as a ts object or a numeric vector.
#' @param mdl The model with arguments included, as returned by \code{setArgs} or \code{setArgsList}.
#' @param draws (Optional, default 0) If nonzero, the Hessian of the log posterior is computed and this
#'   many draws from the corresponding multivariate normal approximation to the posterior are return.
#' @param ... Any additional arguments to pass to \code{rstan::optimizing}.
#' @return The object containing the results of estimation. (See \code{rstan::optimizing}.)
#'
map_estimate <- function(y, mdl, draws=0, ...) {
  standata <- estimation_data(y, mdl$args)
  fit <- rstan::optimizing(mdl$stanmodel, data=standata, as_vector=FALSE,
                           hessian=(draws > 0), draws=draws, ...)
  if (fit$return_code != 0) {
    message(sprintf("MAP estimation failed with code %d.", fit$return_code))
  }
  fit
}

estimation_data <- function(y, modelArgs) {
  y <- as.vector(y)
  c(list(N=length(y), y=y), modelArgs)
}

#' Extract posterior sample of model parameters from estimation results.
#'
#' @export
#' @param fit A value returned by \code{hmc_estimate()}, \code{vb_estimate()}, or \code{map_estimate()}.
#' @param ndraws (optional) number of posterior draws to extract. If not specified,
#'   defaults to the number of draws taken during estimation.
#'
posterior_sample <- function(fit, ndraws = NULL) {
  stopifnot(is.null(ndraws) || ndraws >= 1)
  if ('stanfit' %in% class(fit)) {
    samp <- rstan::extract(fit)
    ndraws_samp <- length(samp$lp__)
    samp$lp__ <- NULL
    if (is.null(ndraws) || ndraws > ndraws_samp)
      ndraws <- ndraws_samp
    idx  <- sample(1:ndraws_samp, ndraws)
    post_samp <- lapply(samp, function(x){abind::asub(x, idx, 1, drop=FALSE)})
  }
  else if ('list' %in% class(fit) && 'par' %in% names(fit)) {
    post_samp <- lapply(fit$par, function(x){
      cl <- class(x)
      if ('array' %in% cl || 'matrix' %in% cl)
        array(x, dim=c(1, dim(x)))
      else if ('numeric' %in% cl)
        x
      else
        stop('Invalid class list in fit$par: ', paste0(cl, collapse=', '))
    })
    ndraws <- 1
  }
  else {
    stop("Invalid fit object passed to posterior_sample().")
  }
  stopifnot(!('ndraws' %in% names(post_samp)))
  c(list(ndraws=ndraws), post_samp)
}

#' Log-likelihood for average of models.
#'
#' @export
#' @param models List of SSMs corresponding to posterior parameter draws
#' @param y Sequence of observed values
#' @return Log likelihood of y given average of models.
#'
averagedLL <- function(models, y) {
  # dlmLL returns negative of log likelihood
  const_term <- - length(y) * 0.5 * log(2 * pi)
  log_mean_exp(-vapply(models, function(mdl){dlm::dlmLL(y, mdl)}, 0.0)) + const_term
}

log_mean_exp <- function(lls) {
  maxll <- max(lls)
  # To avoid underflow and overflow:
  # log(mean(exp(lls)))
  # == log(mean(exp(maxll) * exp(lls - maxll)))
  # == log(exp(maxll) * mean(exp(lls - maxll)))
  # == maxll + log(mean(exp(lls - maxll)))
  maxll + log(mean(exp(lls - maxll)))
}

update_filtered_model <- function(fres) {
  n1 <- NROW(fres$m) # works for vectors, matrices, arbitrary arrays
  new_model <- fres$mod
  dlm::m0(new_model) <- abind::asub(fres$m, n1, 1)
  dlm::C0(new_model) <- dlm::dlmSvd2var(fres$U.C[[n1]], fres$D.C[n1, ])
  new_model
}

#' Apply Kalman filter to each of a list of SSMs.
#'
#' @export
#' @param y Time series (ts object or numeric vector) of observations.
#' @param models List of SSMs.
filter_models <- function(y, models) {
  lapply(models, function(model){dlm::dlmFilter(y, model)})
}

#' Update latent state distributions for list of SSMs.
#'
#' @export
#' @param y Time series (ts object or numeric vector) of new observations. Omit
#'   if \code{filtered} is supplied.
#' @param models List of state-space models. Omit if \code{filtered} is
#'   supplied.
#' @param filtered Result of \code{filter_models}(y, models). Omit if \code{y}
#'   and \code{models} are supplied.
#' @return List of updated models.
update_models <- function(y, models, filtered) {
  if (missing(filtered))
    filtered <- filter_models(y, models)
  lapply(filtered, update_filtered_model)
}

#' Create smoothed estimates of SSM components
#'
#' @export
#' @param y Observed time series.
#' @param filtered Result of \code{filter_models(y, models)}.
#' @param Zcomp m-column matrix, where m is length of latent state vector.
#'   Zcomp[i, ] %*% alpha_t selects component i of the SSM at time t when
#'   alpha_t is the latent state at time t.
#' @return List of two leny x nmdl x ncomp arrays \code{means} and \code{stddevs},
#'   where leny is \code{length(y)}, nmdl is \code{length(filtered)}, and
#'   ncomp is \code{nrow(Zcomp)}.
#'   \code{means}[t,k,i] and \code{stddevs}[t,k,i] are the posterior mean and
#'   standard deviation for component i of model k at time t.
smoothed_components <- function(y, filtered, Zcomp, drop=FALSE) {
  nmdl <- length(filtered)
  stopifnot(nmdl > 0)
  nstate <- length(dlm::m0(filtered[[1]]$mod))
  stopifnot(all(unlist(lapply(filtered, function(fres){
                                        length(dlm::m0(fres$mod)) == nstate}))))
  leny <- length(y)
  leny1 <- leny + 1

  ncomp <- nrow(Zcomp)
  state_means <- array(NA, dim=c(leny, nmdl, nstate))
  state_vars <- array(NA, dim=c(nstate, leny, nmdl, nstate))
  for (k in 1:nmdl) {
    x <- dlm::dlmSmooth(filtered[[k]])
    state_means[ , k, ] <- as.vector(if (nstate == 1) x$s[-1] else x$s[-1, ])
    Varr <- array(data=unlist(dlm::dlmSvd2var(x$U.S, x$D.S)[-1]),
                  dim=c(nstate, nstate, leny))
    state_vars[ , , k, ] <- aperm(Varr, c(1,3,2))
  }

  state_means_mat <- matrix(state_means, nr=leny*nmdl, nc=nstate)
  means <- array(state_means_mat %*% t(Zcomp), dim=c(leny, nmdl, ncomp))

  state_vars_mat <- matrix(state_vars, nr=nstate*leny*nmdl, nc=nstate)
  sds <- array(NA, dim=c(leny, nmdl, ncomp))
  tmp <- state_vars_mat %*% t(Zcomp)
  for (i in 1:ncomp) {
    vars <- Zcomp[i, ] %*% matrix(tmp[,i], nr=nstate, nc=leny*nmdl)
    sds[,,i] <- matrix(sqrt(vars), nr=leny, nc=nmdl)
  }
  if (drop && ncomp == 1) {
    means <- abind::adrop(means, 3) # drop 3rd dimension only
    sds <- abind::adrop(sds, 3)
  }

  list(means = means, stddevs = sds)
}

matEqual <- function(M1, M2) {
  is.matrix(M1) && is.matrix(M2) && dim(M1) == dim(M2) && all(M1 == M2)
}

vapplyL <- function(...) { vapply(..., FUN.VALUE = FALSE) }

#' Create smoothed version of time series.
#'
#' @export
#' @param y Observed time series.
#' @param filtered Result of \code{filter_models(y, models)}.
#' @return List of two leny x nmdl matrices \code{means} and \code{stddevs},
#'   where leny is \code{length(y)} and nmdl is \code{length(models)}.
#'   \code{means}[t,k] and \code{stddevs}[t,k] are the posterior mean and
#'   standard deviation for Z %*% alpha for model k at time t.
smoothed_ts <- function(y, filtered) {
  n <- length(filtered)
  # Must have at least one (filtered) model
  stopifnot(n > 0)
  # Observation matrix must be time-invariant
  stopifnot(all(vapplyL(filtered, function(f){ all(f$mod$JFF == 0) })))
  # All models must have same observation matrix
  FF1 <- filtered[[1]]$mod$FF
  stopifnot(all(vapplyL(filtered[-1], function(f){ matEqual(f$mod$FF, FF1) })))
  # All models must have scalar observations
  stopifnot(nrow(FF1) == 1)

  smoothed_components(y, filtered, FF1, drop=TRUE)
}

### ESTIMATION AND POSTERIOR CHECKS ###
if (FALSE) {
# - ytrain is the observed time series to use for training.
# - ytest is a holdout sequence of additionak observations to use for testing.
# - npost is number of posterior parameter draws to use in creating SSMs.
# - Zc is the component matrix whose rows correspond to the summed components
#   of the SSM. (Summing the rows of Zc gives the 1 x m Z matrix for the SSM.)
#
sm <- cksCompile("my_model.cks", "createSSMs")
source("my_model.R")
margs <- mdlArgs(model, arguments, go, here, ...)
sma <- setArgs(sm, margs)
fit <- hmc_estimate(ytrain, sma)
post <- posterior_sample(fit)
models0 <- createSSMs(margs, post, npost)
filtered0 <- filter_models(ytrain, models0)
models <- update_models(filtered = filtered0)

# Use lltrain and lltest to compare alternative models
lltrain <- averagedLL(models0, ytrain) #F?
lltest <- averagedLL(models, ytest)
# Compare yrep[,i], 1 <= i <= npost, to ytrain; are they visually similar?
# Can also compute various summary statistics T() and check that T(ytrain) is
# within range of values T(yrep[,i]).
yrep <- forecast_sample(models0, length(ytrain), 1)
# Get mean and 90% interval for Z %*% alpha[t], 1 <= t <= length(ytrain)
# Check that smoothed time series looks reasonable.
s <- smoothed_ts(ytrain, filtered0)
ysmooth <- average_normals(s$means, s$stddevs, 0.1, TRUE)
# Get one-step predictive residuals.
res <- lapply(filtered, residuals, type='raw', sd=FALSE)

# decomposition
sc <- smoothed_components(ytrain, filtered0, Zc)
csmooth <- sapply(1:nrow(Zc), function(i){
              average_normals(sc$means[,,i], sc$stddevs[,,i], 0.1, TRUE)})
}

