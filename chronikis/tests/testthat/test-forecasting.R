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

context("Forecasting")
library(chronikis)
#library(rstan)

test_that("average_normals with one alpha, drop=TRUE works", {
  nsteps <- 2
  ndraws <- 3
  means <- matrix(c(-0.24, -4.22,
                    1.06, 1.77,
                    -1.40, 5.33), nr=nsteps, nc=ndraws)
  stddevs <- matrix(c(0.75, 0.36,
                      2.53, 0.67,
                      1.82, 1.57), nr=nsteps, nc=ndraws)
  alpha <- 0.10
  x <- average_normals(means, stddevs, alpha, drop=TRUE)
  expect_equal(sort(names(x)), c('lower','mean','upper'))
  expect_equal(x$mean, c(-0.19333333333333333333, 0.96000000000000000000))
  expect_equal(length(x$lower), nsteps)
  expect_equal(length(x$upper), nsteps)
  expect(is.vector(x$lower), "x$lower should be a vector")
  expect(is.vector(x$upper), "x$upper should be a vector")
  expect_equal(mean(pnorm(rep(x$lower, ndraws), means, stddevs)), 0.05)
  expect_equal(mean(pnorm(rep(x$upper, ndraws), means, stddevs, lower.tail = FALSE)), 0.05)
})

test_that("average_normals with one alpha, drop=FALSE works", {
  nsteps <- 2
  ndraws <- 3
  means <- matrix(c(-0.24, -4.22,
                    1.06, 1.77,
                    -1.40, 5.33), nr=nsteps, nc=ndraws)
  stddevs <- matrix(c(0.75, 0.36,
                      2.53, 0.67,
                      1.82, 1.57), nr=nsteps, nc=ndraws)
  alpha <- 0.10
  x0 <- average_normals(means, stddevs, alpha, drop=TRUE)
  x1 <- average_normals(means, stddevs, alpha, drop=FALSE)
  expect_equal(names(x1), names(x0))
  expect_equal(x1$mean, x0$mean)
  expect_equal(x1$lower, matrix(x0$lower, nc=1))
  expect_equal(x1$upper, matrix(x0$upper, nc=1))
})

test_that("average_normals with multiple alphas works", {
  nsteps <- 2
  ndraws <- 3
  means <- matrix(c(-0.24, -4.22,
                    1.06, 1.77,
                    -1.40, 5.33), nr=nsteps, nc=ndraws)
  stddevs <- matrix(c(0.75, 0.36,
                      2.53, 0.67,
                      1.82, 1.57), nr=nsteps, nc=ndraws)
  alpha <- c(0.20, 0.10, 0.05, 0.01)
  x0 <- lapply(alpha, function(a)average_normals(means, stddevs, a))
  x1 <- average_normals(means, stddevs, alpha)
  expect_equal(names(x1), names(x0[[1]]))
  expect_equal(x1$means, x0[[1]]$means)
  expect_equal(x1$lower, cbind(x0[[1]]$lower, x0[[2]]$lower, x0[[3]]$lower, x0[[4]]$lower))
  expect_equal(x1$upper, cbind(x0[[1]]$upper, x0[[2]]$upper, x0[[3]]$upper, x0[[4]]$upper))
})

test_that("filter_models relates to dlmFilter", {
  y <- rnorm(10)
  models <- list(dlm::dlmModPoly(), dlm::dlmModARMA(0.8))
  f <- filter_models(y, models)
  expect_equal(f, list(dlm::dlmFilter(y, models[[1]]), dlm::dlmFilter(y, models[[2]])))
})

test_that("update_models works for single model", {
  # This test uses a model that is equivalent to a normal distribution with
  # unknown mean
  sigmah <- 2
  sigma0 <- 10
  mu0 <- 5
  n <- 20
  models <- list(dlm::dlm(FF=1, V=sigmah^2, GG=1, W=0, m0=mu0, C0=as.matrix(sigma0^2)))
  y <- rnorm(n, 8, sigmah)
  models1 <- update_models(y, models)
  models2 <- update_models(filtered=filter_models(y, models))
  expect_equal(models1, models2)

  # Use standard Bayesian formula for inferring an unknown mean of a normal distribution
  sigma1sqr = 1 / (1/sigma0^2 + n/sigmah^2)
  mu1 <- sigma1sqr * (mu0/sigma0^2 + n*mean(y)/sigmah^2)
  models3 <- list(dlm::dlm(FF=1, V=sigmah^2, GG=1, W=0, m0=mu1, C0=as.matrix(sigma1sqr)))
  expect_equal(models1, models3)
})

test_that("update_models works for multiple models", {
  models <- list(dlm::dlmModPoly(), dlm::dlmModARMA(0.8))
  y <- rnorm(20, 0, 1)
  models1 <- update_models(y, models)
  models2 <- update_models(filtered=filter_models(y, models))
  expect_equal(models1, models2)
  models3 <- list(update_models(y, models[1])[[1]],
                  update_models(y, models[2])[[1]])
  expect_equal(models1, models3)
})

test_that("forecast_intervals works for one alpha, drop=TRUE", {
  sigma2h <- c(1.5, 2.7)^2
  sigma2rw <- c(0.8, 0.4)^2
  mu <- c(5, 10)
  slope <- c(0.3, 0.5)
  sigma2a0 <- c(12, 8)^2
  # constant trend + random walk
  models <- lapply(1:2, function(i){
    dlm::dlmModPoly(order=2, dV=sigma2h[i], dW=c(sigma2rw[i], 0),
                    m0=c(mu[i], slope[i]), C0=diag(c(sigma2a0[i], 0)))
  })
  nsteps <- 7
  x <- forecast_intervals(models, nsteps, 0.10, drop=TRUE)

  expect_equal(sort(names(x)), c("lower", "mean", "upper"))
  expect_equal(length(x$mean), nsteps)
  expect_equal(length(x$lower), nsteps)
  expect_equal(length(x$upper), nsteps)
  expect(is.vector(x$lower), "x$lower should be a vector")
  expect(is.vector(x$upper), "x$upper should be a vector")

  means <- cbind(mu[1] + slope[1] * (1:nsteps),
                 mu[2] + slope[2] * (1:nsteps))
  stddevs <- sqrt(cbind(sigma2a0[1] + sigma2rw[1] * (1:nsteps) + sigma2h[1],
                        sigma2a0[2] + sigma2rw[2] * (1:nsteps) + sigma2h[2]))
  expect_equal(x$mean, rowMeans(means))
  expect_equal(rowMeans(pnorm(x$lower, means, stddevs)), rep(0.05, nsteps))
  expect_equal(rowMeans(pnorm(x$upper, means, stddevs, lower.tail=FALSE)),
               rep(0.05, nsteps))
})

test_that("forecast_intervals works for one alpha, drop=FALSE", {
  sigma2h <- c(1.5, 2.7)^2
  sigma2rw <- c(0.8, 0.4)^2
  mu <- c(5, 10)
  slope <- c(0.3, 0.5)
  sigma2a0 <- c(12, 8)^2
  # constant trend + random walk
  models <- lapply(1:2, function(i){
    dlm::dlmModPoly(order=2, dV=sigma2h[i], dW=c(sigma2rw[i], 0),
                    m0=c(mu[i], slope[i]), C0=diag(c(sigma2a0[i], 0)))
  })
  nsteps <- 7
  x <- forecast_intervals(models, nsteps, 0.10, drop=FALSE)
  expect_equal(dim(x$lower), c(nsteps, 1))
  expect_equal(dim(x$upper), c(nsteps, 1))
})

test_that("forecast_intervals works for multiple alphas", {
  sigma2h <- c(1.5, 2.7)^2
  sigma2rw <- c(0.8, 0.4)^2
  mu <- c(5, 10)
  slope <- c(0.3, 0.5)
  sigma2a0 <- c(12, 8)^2
  # constant trend + random walk
  models <- lapply(1:2, function(i){
    dlm::dlmModPoly(order=2, dV=sigma2h[i], dW=c(sigma2rw[i], 0),
                    m0=c(mu[i], slope[i]), C0=diag(c(sigma2a0[i], 0)))
  })
  nsteps <- 7
  alpha <- c(0.10, 0.05, 0.01)
  x <- forecast_intervals(models, nsteps, alpha)
  expect_equal(sort(names(x)), c("lower", "mean", "upper"))
  xs <- lapply(alpha, function(a)forecast_intervals(models, nsteps, a))
  expect_equal(x$mean, xs[[1]]$mean)
  expect_equal(x$lower, cbind(xs[[1]]$lower, xs[[2]]$lower, xs[[3]]$lower))
  expect_equal(x$upper, cbind(xs[[1]]$upper, xs[[2]]$upper, xs[[3]]$upper))
})

test_that("forecast_sample returns a result of the right shape", {
  models <- list(dlm::dlmModPoly(), dlm::dlmModSeas(5))
  nsteps <- 4
  res <- forecast_sample(models, nsteps, 3)
  expect_equal(dim(res), c(nsteps, 3*2))
  res <- forecast_sample(models[2], nsteps, 3)
  expect_equal(dim(res), c(nsteps, 3*1))
})

test_that("forecast_sample has correct statistical properties", {
  skip_if(omit_big_test_forecast_sample)
  set.seed(167664666)
  sigma2h <- c(1.5, 2.7)^2
  sigma2rw <- c(0.8, 0.4)^2
  mu <- c(5, 10)
  slope <- c(0.3, 0.5)
  sigma2a0 <- c(12, 8)^2
  # constant trend + random walk
  models <- lapply(1:2, function(i){
    dlm::dlmModPoly(order=2, dV=sigma2h[i], dW=c(sigma2rw[i], 0),
                    m0=c(mu[i], slope[i]), C0=diag(c(sigma2a0[i], 0)))
  })
  nsteps <- 7
  ndpm <- 10000
  draws <- forecast_sample(models, nsteps, ndpm)
  means <- cbind(mu[1] + slope[1] * (1:nsteps),
                 mu[2] + slope[2] * (1:nsteps))
  vars <- cbind(sigma2a0[1] + sigma2rw[1] * (1:nsteps) + sigma2h[1],
                sigma2a0[2] + sigma2rw[2] * (1:nsteps) + sigma2h[2])
  stddevs <- sqrt(vars)
  F <- function(mu,sigma) {
    function(x){0.5 * (pnorm(x, mu[1], sigma[1]) + pnorm(x, mu[2], sigma[2]))}
  }
  # Test marginal distributions
  for (i in c(1, (nsteps+1) %/% 2, nsteps)) {
    res <- ks.test(draws[i,], F(means[i,], stddevs[i,]))
    #cat("\np-value:\n", res$p.value, "\n")
    expect_gt(res$p.value, 0.01)
  }
  # Test distribution of differences
  for (times in list(c(1,4), c(3,7))) {
    delta <- diff(times)
    mu <- delta * slope
    sigma2 <- 2*sigma2h + delta * sigma2rw
    res <- ks.test(draws[times[2], ] - draws[times[1], ], F(mu, sqrt(sigma2)))
    expect_gt(res$p.value, 0.01)
  }
})

