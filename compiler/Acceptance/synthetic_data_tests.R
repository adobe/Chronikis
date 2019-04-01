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

stan_dir <- "../ssm-creation/Acceptance/Reference"

test_wn <- function() {
  sigma <- 150
  N <- 100
  y <- wn(sigma)(N)
  data <- list(N=N, y=y, sigma_scale_=300)
  fit <- rstan::stan(file.path(stan_dir, 'wn.stan'), data=data, chains=4, iter=2000)
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
}

test_llrw <- function(N=100) {
  sigma_q <- 10; sigma_q_scale <- 15
  sigma_h <- 13; sigma_h_scale <- 20
  y0 <- 75; mu_a <- 100; sigma_a <- 30
  y <- llrw(sigma_q, sigma_h, y0)(N)
  data <- list(N=N, y=y,
               sigma_q_scale_=sigma_q_scale, sigma_h_scale_=sigma_h_scale,
               mu_a_=mu_a, sigma_a_=sigma_a)
  fit <- stan(file.path(stan_dir, 'llrw.stan'), data=data, chains=4, iter=2000)
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
  fit
}

test_lltrw <- function(N=100, adapt_delta=0.8) {
  sigma_qa <- 10; sigma_qa_scale <- 20
  sigma_qd <- 2; sigma_qd_scale <- 4
  sigma_h <- 15; sigma_h_scale <- 30
  y0 <- 75; mu_a <- 100; sigma_a <- 50
  d0 <- 0.1; mu_d <- 0; sigma_d <- 50
  y <- lltrw(sigma_qa, sigma_qd, sigma_h, y0, d0)(N)
  data <- list(N=N, y=y,
               sigma_qa_scale_=sigma_qa_scale, sigma_qd_scale_=sigma_qd_scale,
               sigma_h_scale_=sigma_h_scale, mu_a_=mu_a, sigma_a_=sigma_a,
               mu_d_=mu_d, sigma_d_=sigma_d)
  fit <- stan(file.path(stan_dir, 'lltrw.stan'), data=data, chains=4, iter=2000,
              control=list(adapt_delta=adapt_delta))
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
  fit
}

test_llar1_a <- function(N=100, adapt_delta=0.8) {
  rho <- 0.1; rho_mean <- 0.15
  mu <- 100; sigma_a <- 50
  mumu <- 80; sigmamu <- 20
  sigma_h <- 15; sigma_h_scale <- 30
  y <- llar1(rho, mu, sigma_a, sigma_h)(N)
  data <- list(N=N, y=y,
               rho_mean_=rho_mean,
               mu_=mu, sigma_=sigma_a,
               sigma_h_scale_=sigma_h_scale)
  fit <- stan(file.path(stan_dir, 'llar1-a.stan'), data=data, chains=4, iter=2000,
              control=list(adapt_delta=adapt_delta))
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
  fit
}

test_llar1_b <- function(N=100, adapt_delta=0.8) {
  rho <- 0.1; rho_mean <- 0.15
  mu <- 100; mumu <- 80; sigmamu <- 40
  sigma_a <- 50; sigma_total <- sqrt(sigma_a^2 + sigmamu^2)
  sigma_h <- 15; sigma_h_scale <- 30
  y <- llar1(rho, mu, sigma_a, sigma_h)(N)
  data <- list(N=N, y=y,
               rho_mean_=rho_mean,
               mu_=mumu, sigma_total_=sigma_total,
               sigma_h_scale_=sigma_h_scale)
  fit <- stan(file.path(stan_dir, 'llar1-b.stan'), data=data, chains=4, iter=2000,
              control=list(adapt_delta=adapt_delta))
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
  fit
}

test_lltar1_a <- function(N=100, adapt_delta=0.8) {
  rho_b <- 0.2; rho_b_mean <- 0.15
  sigma_b <- 10; sigma_b_scale <- 15
  
  sigma_h <- 7.5; sigma_h_scale <- 15

  y0 <- 100; mu_a <- 80; sigma_a <- 60
  
  rho_d <- 0.09; rho_d_mean <- 0.20
  sigma_d <- 1.0
  
  y <- lltar1(rho_d, sigma_d, rho_b, sigma_b, sigma_h, y0)(N)
  data <- list(N=N, y=y,
               rho_d_mean_=rho_d_mean, sigma_d_=sigma_d,
               mu_a_=mu_a, sigma_a_=sigma_a,
               rho_b_mean_=rho_b_mean, sigma_b_scale_=sigma_b_scale,
               sigma_h_scale_=sigma_h_scale)
  fit <- stan(file.path(stan_dir, 'lltar1-a.stan'), data=data, chains=4, iter=2000,
              control=list(adapt_delta=adapt_delta))
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
  list(fit=fit, y=y)
}

test_diwperiod <- function(N=100, adapt_delta=0.8) {
  sigma_q <- 10; sigma_q_scale=50
  sigma_h <- 10; sigma_h_scale=40
  y0 <- 100; mu_a <- 150; sigma_a <- 75
  rho <- 0.1; rho_mean <- 0.15
  sigma_p <- 30; sigma_p_scale <- 50
  y <- diwperiod(sigma_q, sigma_h, rho, sigma_p, y0)(N)
  data <- list(N=N, y=y,
               sigma_q_scale_=sigma_q_scale,
               sigma_h_scale_=sigma_h_scale,
               mu_a_=mu_a, sigma_a_=sigma_a,
               rho_mean_=rho_mean, sigma_p_scale_=sigma_p_scale)
  fit <- stan(file.path(stan_dir, 'day-in-week-periodicity.stan'), data=data, chains=4, iter=2000,
              control=list(adapt_delta=adapt_delta))
  check_hmc_diagnostics(fit)
  print(summary(fit)$summary)
  list(fit=fit, y=y)
}

