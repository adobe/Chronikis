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

# Utilities used in generated R code that creates SSMs from each draw of
# parameters from the posterior distribution.

#' Create a block diagonal matrix from a 3D array; arr[i, , ] is block i
#' of the resulting matrix.
#' Utility function used by generated R code.
#' @export
bdiag_from_arr3 <- function(arr) {
  dlm::bdiag(lapply(1:dim(arr)[1], function(i)arr[i, , ]))
}

#' Create a diagonal matrix from its diagonal.
#' Utility function used by generated R code.
#' @export
diagv <- function(v) { diag(v, nrow=length(v)) }

#' Number of draws in posterior sample
#' @export
num_draws <- function(postSample) {
  postSample$ndraws
}

polynomial <- function(x, coeffs) {
  xpow <- x ^ (0 : (length(coeffs) - 1))
  sum(coeffs * xpow)
}

#' Rate parameter for exponential distribution truncated to [0,1] with given mean.
#' Utility function used by generated R code.
#' @export
exponential_mt_rate <- function(normalized_mean) {
  # Copied from Stan code, with minimal modification
  if (normalized_mean <= 0) {
    stop("exponential_mt_rate: parameter must be positive, but is ",
           normalized_mean, ".")
  }
  if (normalized_mean >= 1) {
    stop("exponential_mt_rate: parameter must be less than 1, but is ",
           normalized_mean, ".")
  }
  if (normalized_mean > 0.5) {
    mu <- 1.0 - normalized_mean
    sign <- -1.0
  }
  else {
    mu <- normalized_mean
    sign <- 1.0
  }
  if (mu < 4.54545451755986457121e-02)
    res0 <- 1.0 / mu
  else if (mu < 2.10988429519736647721e-01)
    res0 <- polynomial(2.10988429519736647721e-01 - mu,
                      c(9.49448609949615329739e-01,
                        1.04866432774455753396e+00,
                        -6.42959435928104205971e+00,
                        3.79762444624978590113e+00,
                        6.11881450074166792774e+01,
                        -1.48309894287500156906e+02,
                        -2.97228418317228170054e+03,
                        6.97728216979455610272e+04,
                        -4.46472170428893645294e+05,
                        8.96230566675862530246e+05 )) / mu
  else
    res0 <- polynomial(0.5 - mu,
                      c(-3.36262872290467608517e-06,
                        1.20012123407418513921e+01,
                        -1.06585476744743931632e-01,
                        3.27768615844976523022e+01,
                        -7.77323727482908424236e+01,
                        9.86330022949583849368e+02,
                        -5.95443311922654356749e+03,
                        2.45388908776985881559e+04,
                        -5.40960590256227224017e+04,
                        5.49423597728985769209e+04 ))
  return (sign * res0)
}
