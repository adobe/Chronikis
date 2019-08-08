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

context("SSM parameter estimation")
library(chronikis)
library(rstan)

msg <- function(...)cat("\n", ..., "\n")

test_that("NROW gets length along first dimension", {
  # Used in update_filtered_model
  expect_equal(NROW(1:5), 5)
  expect_equal(NROW(matrix(1:6, nr=3)), 3)
  expect_equal(NROW(array(1:24, dim=c(2,3,4))), 2)
})

test_that("mdlArgs works", {
  expect_equal(mdlArgs(), list())
  expect_equal(mdlArgs(a=3), list(a_=3))
  expect_equal(mdlArgs(b=4, c=5), list(b_=4, c_=5))
})

test_that("Full estimation process succeeds", {
  skip_if(omit_test_estimation)
  testdir <- tempfile()
  dir.create(testdir)
  on.exit(unlink(testdir, recursive = TRUE))

  set.seed(104657490)
  ntrain <- 1000
  ntest <- 100
  sigma_q_scale <- 10

  mu_a <- 0
  sigma_a <- 100
  y0 <- rnorm(1, mu_a, sigma_a)
  sigma_q <- abs(rnorm(1, 0, sigma_q_scale))
  yall <- y0 + cumsum(rnorm(ntrain + ntest, 0, sigma_q))
  ytrain <- yall[1:ntrain]
  ytest <- yall[-(1:ntrain)]

  model_code <- '
def main(sigma_q_scale: real{0.0,}, mu_a: real, sigma_a: real{0.0,}) =
  sigma_q ~ half_normal(sigma_q_scale);
  accum(wn(sigma_q), mu_a, sigma_a)
'
  cks_fpath <- file.path(testdir, "testmodel.cks")
  writeChar(model_code, cks_fpath, eos=NULL)
  sm <- cksCompile(cks_fpath, "createSSMs")
  source(file.path(testdir, "testmodel.R"))

  margs <- mdlArgs(sigma_q_scale=sigma_q_scale, mu_a=mu_a, sigma_a=sigma_a)
  sma <- setArgs(sm, margs)
  fit <- hmc_estimate(ytrain, sma, chains=4, iter=2000, warmup=1000)

  post <- posterior_sample(fit)
  # Check sigma_q
  cump <- mean(post$sigma_q < sigma_q)
  q <- quantile(post$sigma_q, c(0.05, 0.95))
  msg("cum prob:", cump)
  expect_lt(cump, 0.975)
  expect_gt(cump, 0.025)
  relrng <- diff(q) / sigma_q
  msg("relative range:", relrng)
  expect_lt(relrng, 0.10)

  # Check sample size
  expect_equal(post$ndraws, 4000)
  expect_equal(length(post$sigma_q), 4000)
  post <- posterior_sample(fit, 5000)
  expect_equal(post$ndraws, 4000)
  expect_equal(length(post$sigma_q), 4000)
  post <- posterior_sample(fit, 100)
  expect_equal(post$ndraws, 100)
  expect_equal(length(post$sigma_q), 100)

  models0 <- createSSMs(margs, post)
  expect_equal(length(models0), post$ndraws)
  models0 <- createSSMs(margs, post, 10)
  expect_equal(length(models0), 10)
  for (i in 1:10) {
    mdl <- models0[[i]]
    sigma_q <- post$sigma_q[i]
    expect_equal(dlm::FF(mdl), as.matrix(1.0))
    expect_equal(dlm::V(mdl), as.matrix(0.0))
    expect_equal(dlm::GG(mdl), as.matrix(1.0))
    expect_equal(dlm::W(mdl), as.matrix(sigma_q^2))
    expect_equal(dlm::m0(mdl), mu_a)
    expect_equal(dlm::C0(mdl), as.matrix(sigma_a^2))
    expect_equal(dlm::JFF(mdl), NULL)
    expect_equal(dlm::JV(mdl), NULL)
    expect_equal(dlm::JGG(mdl), NULL)
    expect_equal(dlm::JW(mdl), NULL)
  }

  # Basic checks for VB
  fit <- vb_estimate(ytrain, sma, output_samples=900)
  post <- posterior_sample(fit)
  # Check sigma_q
  cump <- mean(post$sigma_q < sigma_q)
  q <- quantile(post$sigma_q, c(0.05, 0.95))
  msg("cum prob (vb):", cump)
  expect_lt(cump, 0.975)
  expect_gt(cump, 0.025)
  relrng <- diff(q) / sigma_q
  msg("relative range (vb):", relrng)
  expect_lt(relrng, 0.10)
  # Sample size
  expect_equal(post$ndraws, 900)
  expect_equal(length(post$sigma_q), 900)

  # Basic checks for MAP
  fit <- map_estimate(ytrain, sma)
  post <- posterior_sample(fit)
  expect_equal(post$ndraws, 1)
  reldiff <- abs(post$sigma_q - sigma_q) / sigma_q
  msg("reldiff sigma (MAP):", reldiff)
  expect_lt(reldiff, 0.1)
})

test_that("averagedLL works", {
  set.seed(104657490)
  alpha <- rnorm(400, 0, 1.8)
  yobs <- rnorm(400, alpha, 1)
  models <- lapply(c(2,1.5,2.5), function(sigma){
                   dlm::dlmModARMA(
                     ar=1e-9, sigma2=sigma^2, dV=1, m0=0, C0=as.matrix(1)
                  )})
  ll <- averagedLL(models, yobs)
  #msg("ll:", ll)
  # Rough sanity check
  expect_lt(ll, 400 * dnorm(0, 0, sqrt(1.5^2 + 1^2), log=TRUE))
  expect_gt(ll, 400 * dnorm(2*1.8, 0, sqrt(2.5^2 + 1^2), log=TRUE))
  # Test of log_mean_exp covers the rest
})

test_that("smoothed_ts with 1 model works", {
  set.seed(104657490)
  seas_gen <- dlm::dlmModSeas(10, dV=0, dW=rep(0,9), m0=rep(0,9), C0=5^2*diag(9))
  ar1_gen <- dlm::dlmModARMA(ar=0.97, sigma2=(1-0.97^2)*6^2, dV=4^2, m0=0, C0=as.matrix(6^2))
  mdl <- seas_gen + ar1_gen
  yobs <- dlm::dlmForecast(mdl, nAhead=100, sampleNew=1)$newObs[[1]][,1]
  models <- list(mdl)
  filtered <- filter_models(yobs, models)
  s <- smoothed_ts(yobs, filtered)

  # Rough sanity checks
  expect_equal(sort(names(s)), c("means","stddevs"))
  expect_equal(dim(s$means), c(100, 1))
  expect_equal(dim(s$stddevs), c(100, 1))

  sdiff <- yobs - s$means[,1]
  expect_lt(abs(mean(sdiff)), 0.05)
  sdiff_sd <- sd(sdiff)
  expect_gt(sdiff_sd, 0)
  expect_lt(sdiff_sd, 10)
  #plot(sdiff, typ='l')

  sigma <- s$stddevs[,1]
  expect(all(sigma > 0), "sd should be positive")
  expect_lt(mean(sigma), 10)
  expect_lt(max(sigma), 20)
  #plot(sigma, typ='l', ylim=range(c(0,sigma))*1.1)
})

test_that("smoothed_ts with multiple models works", {
  set.seed(104657490)
  gen <- dlm::dlmModSeas(10, dV=0, dW=rep(0,9), m0=rep(0,9), C0=5^2*diag(9))
  yobs <- dlm::dlmForecast(gen, nAhead=100, sampleNew=1)$newObs[[1]][,1]
  models <- list(NULL, NULL)
  for (i in 1:2) {
    mdl <- gen
    dlm::C0(mdl) <- dlm::C0(mdl) * exp(runif(1,0,0.1))
    models[[i]] <- mdl
  }
  expect(!identical(models[[1]], models[[2]]), "Logic error in test")

  s1 <- smoothed_ts(yobs, filter_models(yobs, list(models[[1]])))
  s2 <- smoothed_ts(yobs, filter_models(yobs, list(models[[2]])))
  s <- smoothed_ts(yobs, filter_models(yobs, models))
  expect_equal(s$means, cbind(s1$means, s2$means))
  expect_equal(s$stddevs, cbind(s1$stddevs, s2$stddevs))
})

test_that("smoothed_components with one model works", {
  set.seed(104657490)
  seas_gen <- dlm::dlmModSeas(10, dV=0, dW=rep(0,9), m0=rep(0,9), C0=5^2*diag(9))
  ar1_gen <- dlm::dlmModARMA(ar=0.97, sigma2=(1-0.97^2)*6^2, dV=4^2, m0=0, C0=as.matrix(6^2))
  mdl <- seas_gen + ar1_gen
  yobs <- dlm::dlmForecast(mdl, nAhead=100, sampleNew=1)$newObs[[1]][,1]
  models <- list(mdl)
  filtered <- filter_models(yobs, models)

  Zcomp <- rbind(c(1, rep(0,9)),   # seasonal
                 c(rep(0,9), 1))   # ar(1)
  sc <- smoothed_components(yobs, filtered, Zcomp)
  s <- smoothed_ts(yobs, filtered)
  expect_equal(sc$means[ , 1, 1] + sc$means[ , 1, 2], s$means[ , 1])
  expect(all(sqrt(sc$stddevs[ , 1, 1]^2 + sc$stddevs[ , 1, 2]^2) > s$stddevs[ , 1]))
  expect_equal(sc$means[11:100, 1, 1], sc$means[1:90, 1, 1]) # periodic component
})

test_that("smoothed_components with multiple models works", {
  N <- 100
  set.seed(104657490)
  seas_gen <- dlm::dlmModSeas(10, dV=0, dW=rep(0,9), m0=rep(0,9), C0=5^2*diag(9))
  ar1_gen <- dlm::dlmModARMA(ar=0.97, sigma2=(1-0.97^2)*6^2, dV=4^2, m0=0, C0=as.matrix(6^2))
  gen <- seas_gen + ar1_gen
  yobs <- dlm::dlmForecast(gen, nAhead=N, sampleNew=1)$newObs[[1]][,1]

  models <- list(NULL, NULL, NULL)
  for (i in 1:3) {
    mdl <- gen
    dlm::C0(mdl) <- dlm::C0(mdl) * exp(runif(1,0,0.1))
    models[[i]] <- mdl
  }

  Zcomp <- rbind(c(1, rep(0,9)),   # seasonal
                 c(rep(0,9), 1))   # ar(1)
  sc1 <- smoothed_components(yobs, filter_models(yobs, list(models[[1]])), Zcomp)
  sc2 <- smoothed_components(yobs, filter_models(yobs, list(models[[2]])), Zcomp)
  sc3 <- smoothed_components(yobs, filter_models(yobs, list(models[[3]])), Zcomp)
  sc <- smoothed_components(yobs, filter_models(yobs, models), Zcomp)

  expect_equal(dim(sc1$means), c(100,1,2))
  expect_equal(dim(sc$means), c(100,3,2))
  combined_means <- abind::abind(sc1$means, sc2$means, sc3$means, along=2)
  attr(combined_means, "dimnames") <- NULL
  expect_equal(sc$means, combined_means)

  expect_equal(dim(sc1$stddevs), c(100,1,2))
  expect_equal(dim(sc$stddevs), c(100,3,2))
  combined_stddevs <- abind::abind(sc1$stddevs, sc2$stddevs, sc3$stddevs, along=2)
  attr(combined_stddevs, "dimnames") <- NULL
  expect_equal(sc$stddevs, combined_stddevs)
})

test_that("log_mean_exp (averagedLL) works", {
  expect_equal(0, log_mean_exp(rep(0, 3)))
  expect_equal(-10000, log_mean_exp(rep(-10000, 3)))
  expect_equal(10000, log_mean_exp(rep(10000, 3)))

  x <- 10 + c(log(0.5), log(0.5), log(1), log(2)) # avg of 0.5,0.5,1,2 is 1
  expect_equal(log_mean_exp(x), 10)
})

test_that("posterior_sample works for HMC", {
  # No separate test for VB since it also returns a stanfit object.
  fit <- readRDS('fit1.rds')
  samp0 <- readRDS('hmcsamp1.rds')
  idx0 <- sort.int(samp0$real_a, index.return=TRUE)$ix

  samp1 <- posterior_sample(fit$hmc)
  samp2 <- posterior_sample(fit$hmc, 11)
  for (samp in list(samp1, samp2)) {
    idx <- sort.int(samp$real_a, index.return = TRUE)$ix
    expect_equal(class(samp), "list")
    expect_equal(sort(names(samp)), c("mat_c", "ndraws", "real_a", "vec_b"))
    expect_equal(samp$ndraws, 10)
    expect_equal(sort(samp$real_a), samp0$real_a[idx0])
    expect_equal(samp$vec_b[idx, ], samp0$vec_b[idx0, ])
    expect_equal(samp$mat_c[idx, , ], samp0$mat_c[idx0, , ])
  }

  samp3 <- posterior_sample(fit$hmc, 3)
  expect_equal(class(samp3), "list")
  expect_equal(sort(names(samp3)), c("mat_c", "ndraws", "real_a", "vec_b"))
  expect_equal(samp3$ndraws, 3)
  pos <- vapply(1:3, function(i)which(samp0$real_a == samp3$real_a[i]), 0L)
  for (i in 1:3)
    expect_equal(length(pos[i]), 1)
  expect_equal(samp3$real_a, samp0$real_a[pos])
  expect_equal(samp3$vec_b, samp0$vec_b[pos, ])
  expect_equal(samp3$mat_c, samp0$mat_c[pos, , ])
})

test_that("posterior_sample works for MAP", {
  real_a_draws0 <- 2.0
  real_a_draws <- 2.0

  vec_b_draws0 <- array(5.0, dim=1L)
  vec_b_draws <- array(5.0, dim=c(1L, 1L))

  vec_c_draws0 <- array((10:11) + 0.1, dim=2L)
  vec_c_draws <- array((10:11) + 0.1, dim=c(1L, 2))

  mat_d_draws0 <- array((1:8) + 0.5, dim=c(2L, 4))
  mat_d_draws <- array((1:8) + 0.5, dim=c(1L, 2, 4))

  arr_e_draws0 <- array((1:24) + 0.25, dim=c(2L, 3, 4))
  arr_e_draws <- array((1:24) + 0.25, dim=c(1L, 2, 3, 4))

  fit <- list(par = list(a = real_a_draws0, b = vec_b_draws0,
                         c = vec_c_draws0, d = mat_d_draws0, e = arr_e_draws0),
              value = 123.45,
              return_code = 0L)
  expected = list(ndraws=1L, a = real_a_draws, b = vec_b_draws,
                  c = vec_c_draws, d = mat_d_draws, e = arr_e_draws)
  expect_equal(posterior_sample(fit), expected)
  expect_equal(posterior_sample(fit, 2), expected)
})
