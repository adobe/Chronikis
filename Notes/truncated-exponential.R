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

# This code was used to find and verify the polynomial approximations
# used for exponential_mt_rate in StanASTImpl.hs

fac <- function(N) {
  prod(1:N)
}

# g(y) = (1/y) * SUM(i: i >= 2: y^i / i!) / SUM(i : i >= 1: y^i / i!)

gratio_denom <- function(y, N) {
  sum <- 0
  for (i in 1:N)
    sum <- sum + y^i / prod(1:i)
  (y^(N+1) / fac(N)) / sum
}

gratio_numer <- function(y, N) {
  sum <- 0
  for (i in 2:N)
    sum <- sum + y^i / prod(1:i)
  (y^(N+1) / fac(N)) / sum
}

gthresh_y <- 0.1
gthresh_N <- 12

test_gratio <- function() {
  stopifnot(gratio_denom(gthresh_y, gthresh_N) <= 1e-19)
  stopifnot(gratio_numer(gthresh_y, gthresh_N) <= 1e-19)
}

g <- function(y) {
  glarge <- function(yy) {
    1/yy - 1/expm1(yy)
  }
  gsmall <- function(yy) {
    N <- gthresh_N
    powers <- 1:N
    factorials <- cumprod(powers)  # 1!, 2!, ..., N!
    coeffs_denom <- 1 / factorials
    coeffs_numer <- coeffs_denom
    coeffs_numer[1] <- 0
    ypowers <- outer(yy, powers, FUN='^')
    numer <- as.vector(ypowers %*% coeffs_numer)
    denom <- as.vector(ypowers %*% coeffs_denom)
    numer / denom / yy
  }
  gy <- rep(NA, length(y))
  gy[y == 0] <- 0.5
  yneg <- (y < 0)
  if (any(yneg))
    gy[yneg] <- 1 - g(-y[yneg])
  ysmall <- (0 < y & y < gthresh_y)
  if (any(ysmall))
    gy[ysmall] <- gsmall(y[ysmall])
  ylarge <- y >= gthresh_y
  if (any(ylarge))
    gy[ylarge] <- glarge(y[ylarge])
  return(gy)
}

test_g <- function() {
  y<- seq(-2*gthresh_y, 2*gthresh_y, length.out = 1e6 + 1)
  delta <- 4 * gthresh_y / 1e6
  y[abs(y) < 0.1 * delta] <- 0
  gy1 <- g(y)
  gy2 <- ifelse(y == 0, 0.5, 1/y - 1/expm1(y))
  relerr <- 2 * abs(gy1 - gy2) / (abs(gy1) + abs(gy2))
  stopifnot(max(relerr) <= 1e-9)
}

thin.plot <- function(x, y) {
  n <- length(x)
  stopifnot(length(y) == n)
  if (n > 1000) {
    k <- n %/% 1000
    idx <- seq(k %/% 2, n, by=k)
    x <- x[idx]
    y <- y[idx]
  }
  plot(x, y, typ='l', ylab='')
}

max_relerr <- function(x, y, show.plot = FALSE) {
  x1 <- g(y)
  relerr <- (x1 - x) / x
  if (show.plot)
    thin.plot(x, relerr)
  max(abs(relerr))
}

xvals <- function(min_x, max_x, N) {
  stopifnot(0 < min_x && min_x < max_x && max_x <= 0.5)
  exp(seq(log(min_x), log(max_x), length.out = N + 1))
}

poly.eval <- function(x, coeffs) {
  y <- coeffs[1] # intercept
  for (i in 2:length(coeffs))
    y <- y + coeffs[i] * (x ^ (i-1))
  return(y)
}

fit.theta.range.1 <- function(min_theta, max_theta, increment) {
  y <- seq(min_theta, max_theta, by=increment)
  x <- g(y)
  max_x <- max(x)
  min_x <- min(x)
  stopifnot(max_x == x[1])
  refx <- max_x
  xd <- refx - x
  fit <- lm(y ~ I(xd) + I(xd^2) + I(xd^3) + I(xd^4) + I(xd^5) + I(xd^6) + I(xd^7) + I(xd^8) + I(xd^9))
  x1 <- xvals(min_x, max_x, 10 * length(y))
  y1 <- poly.eval(refx - x1, fit$coefficients)
  list(
    min_x = min_x,
    max_x = max_x,
    refx = refx,
    max_rel_err = max_relerr(x1, y1),
    coeffs = fit$coefficients
  )
}

fit.theta.range.2 <- function(min_theta, max_theta, increment) {
  stopifnot(min_theta > 0)
  y <- seq(min_theta, max_theta, by=increment)
  x <- g(y)
  max_x <- max(x)
  min_x <- min(x)
  stopifnot(max_x == x[1])
  refx <- max_x
  xd <- refx - x
  r <- x * y
  fit <- lm(r ~ I(xd) + I(xd^2) + I(xd^3) + I(xd^4) + I(xd^5) + I(xd^6) + I(xd^7) + I(xd^8) + I(xd^9))
  x1 <- xvals(min_x, max_x, 10 * length(y))
  r1 <- poly.eval(refx - x1, fit$coefficients)
  y1 <- r1 / x1
  list(
    min_x = min_x,
    max_x = max_x,
    refx = refx,
    max_rel_err = max_relerr(x1, y1),
    coeffs = fit$coefficients
  )
}

iter.soln <- function(x, N) {
  phi.iter <- function(y) {
    (1/x) * (1 - y/expm1(y))
  }
  y <- 1/x
  if (N > 0)
  for (i in 1:N) {
    y <- phi.iter(y)
  }
  maxrelerr <- max(abs(g(y) - x) / x)
  list(y = y, max_rel_err = maxrelerr)
}

gen_approx <- function() {
  approx1 <- fit.theta.range.1(0, 4.5, 1e-6)
  cat("approx1 created\n")
  stopifnot(approx1$max_rel_err <= 1e-6)
  stopifnot(approx1$max_x == 0.5)
  approx2 <- fit.theta.range.2(4.5, 22, 1e-6)
  cat("approx2 created\n")
  stopifnot(approx2$max_rel_err <= 1e-6)
  stopifnot(approx2$max_x == approx1$min_x)
  einv <- iter.soln(xvals(approx2$min_x / 1e6, approx2$min_x, 1e6 - 1), 0)$max_rel_err
  cat("einv computed\n")
  stopifnot(einv <= 1e-6)
  approx_obj <- list(
    x1 = approx2$min_x,
    x2 = approx2$max_x,
    refx_r = approx2$refx,
    coeffs_r = approx2$coeffs,
    refx_y = approx1$refx,
    coeffs_y = approx1$coeffs
  )
  approx_fct <- function(x) {
    stopifnot(all(0 < x & x < 1))
    stopifnot(approx_obj$x1 < approx_obj$x2)
    y <- rep(NA, length(x))
    xlarge <- (x > 0.5)
    if (any(xlarge))
      y[xlarge] <- -approx_fct(1 - x[xlarge])
    xltx1 <- (0 < x & x < approx_obj$x1)
    if (any(xltx1))
      y[xltx1] <- 1 / x[xltx1]
    xinx1x2 <- (approx_obj$x1 <= x & x <= approx_obj$x2)
    if (any(xinx1x2))
      y[xinx1x2] <- poly.eval(approx_obj$refx_r - x[xinx1x2], approx_obj$coeffs_r) / x[xinx1x2]
    xgtx2 <- (approx_obj$x2 < x & x <= 0.5)
    if (any(xgtx2))
      y[xgtx2] <- poly.eval(approx_obj$refx_y - x[xgtx2], approx_obj$coeffs_y)
    stopifnot(all(xlarge | xltx1 | xinx1x2 | xgtx2))
    return(y)
  }
  approx_fct_is_good <- function() {
    x <- seq(1e-6, 1-1e-6, by=1e-6)
    x[abs(x-0.5) < 1e-7] <- 0.5
    y <- approx_fct(x)
    max_rel_err <- max_relerr(x, y)
    return (max_rel_err <= 1e-6)
  }
  stopifnot(approx_fct_is_good())
  list(
    obj = approx_obj,
    fct = approx_fct
  )
}

exponential_mt_stan_code <- function(approx_obj) {
  stopifnot(approx_obj$x1 < approx_obj$x2)
  d <- getOption("digits")
  options(digits=20)
  on.exit(options(digits=d))
  
  main_fct <- '
real exponential_mt_lpdf(real x, real mu, real ub) {
  if (ub <= 0)
    reject("exponential_mt_lpdf: Upper bound parameter must be positive, but is ", ub, ".");
  if (mu <= 0)
    reject ("exponential_mt_lpdf: Mean parameter must be positive, but is ", mu, ".");
  if (mu >= ub)
    reject ("exponential_mt_lpdf: Mean parameter must be less than upper bound ", ub, ", but is ", mu, ".");
  if (x < 0)
    reject("exponential_mt_lpdf: Variable must be nonnegative, but is ", x, ".");
  if (x > ub)
    reject("exponential_mt_lpdf: Variable must be less than upper bound ", ub, ", but is ", x, ".");
  return exponential_lpdf(x | exponential_mt_rate(mu / ub) / ub);
}
'
  polynomial_fct <- '
real polynomial(real x, real coeffs[]) {
  real y = 0;
  real pow = 1;
  for (i in 1:size(coeffs)) {
    y += pow * coeffs[i];
    pow *= x;
  }
  return (y);
}
'
  stopifnot(approx_obj$refx_y == 0.5)
  num_list <- function(v) {
    paste(sprintf("%.20e", v), collapse=", ")
  }
  rate_fct <- sprintf('
real exponential_mt_rate(real normalized_mean) {
  if (normalized_mean <= 0)
    reject("exponential_mt_rate: parameter must be positive, but is ", normalized_mean, ".");
  if (normalized_mean >= 1)
    reject("exponential_mt_rate: parameter must be less than 1, but is ", normalized_mean, ".");
  if (normalized_mean < %.20e)
    return inv(normalized_mean);
  else if (normalized_mean < %.20e)
    return polynomial(%.20e - normalized_mean, { %s }) / normalized_mean;
  else if (normalized_mean <= 0.5)
    return polynomial(0.5 - normalized_mean, { %s });
  else // normalized_mean > 0.5
    return -exponential_mt_rate(1 - normalized_mean);
}
', approx_obj$x1, approx_obj$x2, approx_obj$refx_r
 , num_list(approx_obj$coeffs_r), num_list(approx_obj$coeffs_y))
  return(paste0(polynomial_fct, rate_fct, main_fct))
}
