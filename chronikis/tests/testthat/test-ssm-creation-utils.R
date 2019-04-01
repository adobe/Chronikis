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

context("SSM creation utils")
library(chronikis)

test_that("bdiag_from_arr3 works", {
  arr <- array(1:6, dim=c(1,2,3))
  expected <- matrix(nr=2, nc=3, byrow=TRUE, data=c(
    1, 3, 5,
    2, 4, 6
  ))
  expect_equal(bdiag_from_arr3(arr), expected)

  arr <- array(1:24, dim=c(2, 4, 3))
  expected <- matrix(nr=8, nc=6, byrow=TRUE, data=c(
    1,  9, 17, 0,  0,  0,
    3, 11, 19, 0,  0,  0,
    5, 13, 21, 0,  0,  0,
    7, 15, 23, 0,  0,  0,
    0,  0,  0, 2, 10, 18,
    0,  0,  0, 4, 12, 20,
    0,  0,  0, 6, 14, 22,
    0,  0,  0, 8, 16, 24
  ))
  expect_equal(bdiag_from_arr3(arr), expected)
})

test_that("diagv works", {
  expect_equal(diagv(numeric(0)), matrix(numeric(0), nr=0, nc=0))

  expect_equal(diagv(1), matrix(1, nr=1, nc=1))

  expect_equal(diagv(c(2,3)), rbind(c(2, 0),
                                    c(0, 3)))
})

test_that("exponential_mt_rate works", {
  integr <- function(f) {
    tol <- 1e-8
    r <- cubature::adaptIntegrate(f, 0, 1, maxEval = 0, doChecking = TRUE,
                                  tol = tol)
    stopifnot(!is.na(r$error) && r$returnCode == 0)
    r$integral
  }
  cases <- c(0.001, 0.045, 0.046, 0.210, 0.211, 0.350, 0.499)
  for (mu in cases) {
    theta <- exponential_mt_rate(mu)
    f0 <- function(x){exp(-theta * x)}
    Z <- integr(f0)
    f <- function(x){x * exp(-theta * x) / Z}
    mu1 <- integr(f)
    expect_lt(abs((mu - mu1) / mu), 1e-6)

    thetam <- exponential_mt_rate(1 - mu)
    expect_equal(thetam, -theta)
  }
})
