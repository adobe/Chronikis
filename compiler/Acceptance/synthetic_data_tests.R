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

library(MASS)
library(rstan)
library(chronikis)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

wn <- function(sigma)function(N) {
  rnorm(N, 0, sigma)
}

ar1 <- function(phi, sigmaQ, mu, sigmaA)function(N) {
  y <- rep(NA, N)
  y[1] <- rnorm(1, 0, sigmaA)
  q <- rnorm(N-1, 0, sigmaQ)
  for (i in 2:N)
    y[i] <- phi * y[i-1] + q[i-1]
  mu + y
}

periodic_corr <- function(period, ell) {
  stopifnot(floor(period) == period)
  g <- function(x){exp(-2 * (sin(pi*x/period) / ell)^2)}
  offset <- mean(g(0:(period-1)))
  scale <- 1 / (1 - offset)
  function(delta) {
    scale * (exp(-2 * (sin(pi*delta/period) / ell)^2) - offset)
  }
}

decaying_corr <- function(rho) {
  a <- 0.5 * log1p(-rho^2)
  function(delta) {
    exp(a * delta)
  }
}

phi_of_rho <- function(rho) {
  sqrt(1-rho^2)
}

deltamat <- function(N) {
  abs(matrix(1:N, nrow=N, ncol=N, byrow=TRUE) - matrix(1:N, nrow=N, ncol=N, byrow=FALSE))
}

qpCorr <- function(period, ell, rho) {
  per <- periodic_corr(period, ell)
  dec <- decaying_corr(rho)
  function(N) {
    delta <- deltamat(N)
    per(delta) * dec(delta)
  }
}

qp <- function(period, ell, rho, sigma) {
  stopifnot(floor(period) == period)
  corrFct <- qpCorr(period, ell, rho)
  function(N) {
    sigma * mvrnorm(1, rep(0,N), corrFct(N))
  }
}

accum <- function(gen, y0)function(N) {
  cumsum(c(y0, gen(N-1)))
}

`%+%` <- function(a,b)function(N) {
  a(N) + b(N)
}

llrw <- function(sigmaQ, sigmaH, y0) {
  accum(wn(sigmaQ), y0) %+%
    wn(sigmaH)
}

lltrw <- function(sigmaQA, sigmaQD, sigmaH, y0, d0) {
  accum(accum(wn(sigmaQD), d0) %+% wn(sigmaQA), y0) %+%
    wn(sigmaH)
}

llar1 <- function(rho, mu, sigmaA, sigmaH) {
  phi <- sqrt(1 - rho^2)
  ar1(phi, rho * sigmaA, mu, sigmaA) %+%
    wn(sigmaH)
}

lltar1 <- function(rhoD, sigmaD, rhoB, sigmaB, sigmaH, y0) {
  phiB <- sqrt(1 - rhoB^2)
  phiD <- sqrt(1 - rhoD^2)
  accum(ar1(phiD, rhoD * sigmaD, 0, sigmaD), y0) %+%
    ar1(phiB, rhoB * sigmaB, 0, sigmaB) %+%
    wn(sigmaH)
}

diwperiod <- function(sigmaQ, sigmaH, rho, sigmaP, y0) {
  # Recover sigmaQ, sigmaH, rho, sigmaP
  accum(wn(sigmaQ), y0) %+%
    qp(7, 0.7, rho, sigmaP) %+%
    wn(sigmaH)
}

stan_dir <- "./Reference"

check_parms <- function(samp, expected_parms) {
  checkit <- function(expected, name) {
    name1 <- paste0(name,"_")
    pdraws <- samp[[name1]]
    cat(sprintf("expected %s: %g\n", name, expected))
    cat(sprintf("posterior quantile: %g\n", mean(pdraws <= expected)))
    ci <- quantile(pdraws, c(0.025, 0.975))
    cat(sprintf("posterior CI: %g to %g\n", ci[1], ci[2]))
    cat("\n")
    MASS::truehist(pdraws, xlab=name)
  }
  for (nm in names(expected_parms))
    checkit(expected_parms[nm], nm)
}

plot_forecast <- function(ytrain, ymid, ylo, yhi) {
  n <- length(ytrain)
  start <- n + 1
  y1 <- ts.union(ts(ymid, start), ts(ylo, start), ts(yhi, start))
  y1col <- c('blue', 'blue', 'blue')
  y1lty <- c('solid', 'dashed', 'dashed')
  y <- ts.union(ts(ytrain), y1)
  col <- c('black', y1col)
  lty <- c('solid', y1lty)
  plot(y, plot.type='single', xlab='t', col=col, lty=lty)
  lines(c(n, n+1), c(ytrain[n], ymid[1]), col=y1col[1], lty=y1lty[1])
  lines(c(n, n+1), c(ytrain[n], ylo[1]), col=y1col[2], lty=y1lty[2])
  lines(c(n, n+1), c(ytrain[n], yhi[1]), col=y1col[3], lty=y1lty[3])
}

create_forecast <- function(nsteps, ytrain, margs, fit) {
  post <- posterior_sample(fit)
  npost <- 100
  mname <- fit@model_name
  source(file.path(stan_dir, paste0(mname, '.R')))
  models0 <- do.call(paste0('createSSMs_', gsub('-','.', mname)),
                     list(margs, post, npost))
  filtered0 <- filter_models(ytrain, models0)
  models <- update_models(filtered = filtered0)
  alpha <- 0.10
  forecast_intervals(models, nsteps, alpha, TRUE)
}

create_and_plot_forecast <- function(nsteps, ytrain, margs, fit) {
  fcres <- create_forecast(nsteps, ytrain, margs, fit)
  plot_forecast(ytrain, fcres$mean, fcres$lower, fcres$upper)
}

run_test <- function(mname, nsteps_forecast, y, adapt_delta,
                     inferred_parms, known_parms)
{
  sm <- rstan::stan_model(file.path(stan_dir, paste0(mname, '.stan')))
  margs <- do.call(mdlArgs, known_parms)
  fit <- hmc_estimate(y, setArgs(sm, margs),
                      control=list(adapt_delta=adapt_delta))
  check_hmc_diagnostics(fit)
  x <- summary(fit)$summary[,c('2.5%','50%', '97.5%', 'n_eff', 'Rhat')]
  cat(sprintf('Min Rhat: %g\nMax Rhat: %g\n', min(x[,'Rhat']), max(x[,'Rhat'])))
  cat(sprintf('Min n_eff: %g\n', min(x[,'n_eff'])))
  cat('\n')
  check_parms(rstan::extract(fit), inferred_parms)
  create_and_plot_forecast(nsteps_forecast, y, margs, fit)
  invisible(fit)
}

test_wn <- function(N=100, Nfc=25, adapt_delta=0.8) {
  mu_a <- 500
  sigma_a <- 800
  sigma_scale <- 300
  sigma <- 150
  y <- rnorm(1, mu_a, sigma_a) + wn(sigma)(N)
  run_test('wn', Nfc, y, adapt_delta,
           c(sigma=sigma),
           list(mu_a=mu_a, sigma_a=sigma_a, sigma_scale=sigma_scale))
}

test_llrw <- function(N=100, Nfc=25, adapt_delta=0.8) {
  sigma_q <- 10; sigma_q_scale <- 15
  sigma_h <- 13; sigma_h_scale <- 20
  y0 <- 75; mu_a <- 100; sigma_a <- 30
  y <- llrw(sigma_q, sigma_h, y0)(N)
  run_test('llrw', Nfc, y, adapt_delta,
           c(sigma_q=sigma_q, sigma_h=sigma_h),
           list(sigma_q_scale=sigma_q_scale, sigma_h_scale=sigma_h_scale,
                mu_a=mu_a, sigma_a=sigma_a))
}

test_lltrw <- function(N=200, Nfc=50, adapt_delta=0.8) {
  sigma_qa <- 10; sigma_qa_scale <- 20
  sigma_qd <- 2; sigma_qd_scale <- 4
  sigma_h <- 15; sigma_h_scale <- 30
  y0 <- 75; mu_a <- 100; sigma_a <- 50
  d0 <- 0.1; mu_d <- 0; sigma_d <- 50
  y <- lltrw(sigma_qa, sigma_qd, sigma_h, y0, d0)(N)
  run_test('lltrw', Nfc, y, adapt_delta,
           c(sigma_qa=sigma_qa, sigma_qd=sigma_qd, sigma_h=sigma_h),
           list(sigma_qa_scale=sigma_qa_scale, sigma_qd_scale=sigma_qd_scale,
                sigma_h_scale=sigma_h_scale, mu_a=mu_a, sigma_a=sigma_a,
                mu_d=mu_d, sigma_d=sigma_d))
}

test_llar1_a <- function(N=100, Nfc=25, adapt_delta=0.8) {
  rho <- 0.1; rho_mean <- 0.15
  mu <- 100; sigma <- 50
  sigma_h <- 15; sigma_h_scale <- 30
  y <- llar1(rho, mu, sigma, sigma_h)(N)
  run_test('llar1-a', Nfc, y, adapt_delta,
           c(rho=rho, sigma_h=sigma_h),
           list(rho_mean=rho_mean, mu=mu, sigma=sigma, sigma_h_scale=sigma_h_scale))
}

test_llar1_a1 <- function(N=900, Nfc=225, adapt_delta=0.8) {
  rho <- 0.1; rho_mean <- 0.15
  mu <- 100; sigma <- 150
  m <- rnorm(1, mu, sigma)
  sigma_a <- 50; sigma_a_scale <- 90
  sigma_h <- 15; sigma_h_scale <- 30
  y <- llar1(rho, m, sigma_a, sigma_h)(N)
  fit <-
    run_test('llar1-a1', Nfc, y, adapt_delta,
           c(rho=rho, sigma_h=sigma_h, sigma_a=sigma_a),
           list(rho_mean=rho_mean, mu=mu, sigma=sigma, sigma_a_scale=sigma_a_scale,
                sigma_h_scale=sigma_h_scale))
  cat("m:", m, "\n")
  fit
}


test_llar1_a2 <- function(N=900, Nfc=225, adapt_delta=0.8) {
  sigma_h <- 15; sigma_h_scale <- 30
  mu <- 100; sigma <- 150
  m <- rnorm(1, mu, sigma)
  sigma_rw <- 11; sigma_rw_scale <- 50
  sigma_st <- 50; sigma_st_scale <- 150

  rho <- sigma_rw / sigma_st
  y <- llar1(rho, m, sigma_st, sigma_h)(N)
  fit <-
    run_test('llar1-a2', Nfc, y, adapt_delta,
             c(sigma_h=sigma_h, sigma_rw=sigma_rw,
               sigma_st_extra=sigma_st-sigma_rw),
             list(sigma_rw_scale=sigma_rw_scale, sigma_st_scale=sigma_st_scale,
                  mu=mu, sigma=sigma, sigma_h_scale=sigma_h_scale))
  cat("m:", m, "\n")
  fit
}


test_llar1_b <- function(N=900, Nfc=225, adapt_delta=0.8) {
  rho <- 0.2; rho_mean <- 0.15
  mu <- 100; sigmamu_a <- 40
  mu_a <- rnorm(1, mu, sigmamu_a); 
  sigma_a <- 50; sigma_total <- sqrt(sigma_a^2 + sigmamu_a^2)
  sigma_h <- 15; sigma_h_scale <- 30
  y <- llar1(rho, mu_a, sigma_a, sigma_h)(N)
  fit <-
  run_test('llar1-b', Nfc, y, adapt_delta,
           c(rho=rho, sigma_h=sigma_h, sigmasqr_a=sigma_a^2),
           list(rho_mean=rho_mean, mu=mu, sigma_total=sigma_total,
                sigma_h_scale=sigma_h_scale))
  cat(sprintf("mu_a: %g\n", mu_a))
  fit
}

test_lltar1_a <- function(N=100, Nfc=25, adapt_delta=0.9) {
  rho_b <- 0.2; rho_b_mean <- 0.15
  sigma_b <- 10; sigma_b_scale <- 15
  sigma_h <- 7.5; sigma_h_scale <- 15
  y0 <- 100; mu_a <- 80; sigma_a <- 60
  rho_d <- 0.09; rho_d_mean <- 0.20
  sigma_d <- 1.0
  
  y <- lltar1(rho_d, sigma_d, rho_b, sigma_b, sigma_h, y0)(N)
  run_test('lltar1-a', Nfc, y, adapt_delta,
           c(sigma_h=sigma_h, rho_b=rho_b, sigma_b=sigma_b, rho_d=rho_d),
           list(rho_d_mean=rho_d_mean, sigma_d=sigma_d, mu_a=mu_a, sigma_a=sigma_a,
                rho_b_mean=rho_b_mean, sigma_b_scale=sigma_b_scale, sigma_h_scale=sigma_h_scale))
}

test_diwperiod <- function(N=100, Nfc=25, adapt_delta=0.8) {
#  sigma_q <- 10; sigma_q_scale=50
  sigma_q <- 4; sigma_q_scale=50
#  sigma_h <- 10; sigma_h_scale=40
  sigma_h <- 5; sigma_h_scale=40
  y0 <- 100; mu_a <- 150; sigma_a <- 75
  rho <- 0.1; rho_mean <- 0.15
  sigma_p <- 50; sigma_p_scale <- 40
  y <- diwperiod(sigma_q, sigma_h, rho, sigma_p, y0)(N)
  run_test('day-in-week-periodicity', Nfc, y, adapt_delta,
           c(sigma_q=sigma_q, sigma_h=sigma_h, rho=rho, sigma_p=sigma_p),
           list(sigma_q_scale=sigma_q_scale, sigma_h_scale=sigma_h_scale,
                mu_a=mu_a, sigma_a=sigma_a, rho_mean=rho_mean, sigma_p_scale=sigma_p_scale))
}

