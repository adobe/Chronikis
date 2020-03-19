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

# This code was used to find and verify aspects of the code in Periodic.hs,
# especialy the table mls_table.


geny <- function(L, rho, n, fixed_start=FALSE) {
  phi <- rho^(1/L)
  theta <- 2 * pi / L
  M <- phi * matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2, byrow=TRUE)
  sqrtQ <- sqrt(1 - phi^2)
  y <- rep(NA, n * L + 1)
  alpha <- if (fixed_start) c(0, -1) else rnorm(2)
  for (i in 1:(n*L)) {
    y[i] <- alpha[1]
    alpha <- M %*% alpha + rnorm(2, 0, sqrtQ)
  }
  y[n*L + 1] <- alpha[1]

  y
}

check_rho <- function(L, rho, N) {
  x <- vapply(1:N, function(i){
    y <- geny(L, rho, 1)
    y[1] * y[L+1]
  }, 0)
  mu <- mean(x)
  sigma <- sd(x) / sqrt(N)
  list(mu = mu, sigma = sigma, Z = (rho - mu) / sigma)
}

f <- function(L, rho, n) {
  save <- par('mfrow')
  par(mfrow=c(2,2), mar=c(4,4,1,2)+0.1)
  for (i in 1:4) {
    y <- geny(L, rho, n, fixed_start=TRUE)
    yrng <- c(-1.7, 1.7) # max(abs(range(y))) * c(-1,1)
    plot(1:(n*L + 1), y, ty='l', xlab='', ylab='', ylim=yrng)
    lines(c(1,n*L+1), c(0, 0), col='gray')
    lines(c(1,n*L+1), c(1, 1), col='gray')
    lines(c(1,n*L+1), c(-1, -1), col='gray')
    for (i in 0:n) {
      lines(rep(i*L+1, 2), yrng, col='gray', lty='dashed')
    }
  }
  par(mfrow=save)
}

# --------------------------------------------
library(matrixStats)

periodic_covf <- function(ell)function(x){
  exp(-2 * sin(pi * x)^2 / ell^2)
}

pcovf_std <- function(ell, iperiod, nmax){
  rhos <- periodic_covf(ell)((0:nmax) / iperiod)
  rhos <- rhos - mean(rhos)
  rhos <- rhos / rhos[1]
  stopifnot(rhos[1] == 1)
  stopifnot(abs(sum(rhos)) <= 3 * sqrt(nmax+1) * 1e-15)
  rhos
}

pcf_approx <- function(coeffs)function(x) {
  nr <- length(x)
  nc <- length(coeffs)
  xmat <- matrix(x, nrow=nr, ncol=nc, byrow=FALSE)
  coeffsmat <- matrix(coeffs, nrow=nr, ncol=nc, byrow=TRUE)
  kmat <- matrix((1:nc)-1, nrow=nr, ncol=nc, byrow=TRUE)
  terms <- coeffsmat * cos(2 * pi * kmat * xmat)
  apply(terms, 1, function(x){ sum(sort(x))}) # rowSums, with round-off error minimized
}

pcf_approxs <- function(coeffs, x) {
  nr <- length(x)
  nc <- length(coeffs)
  xmat <- matrix(x, nrow=nr, ncol=nc, byrow=FALSE)
  coeffsmat <- matrix(coeffs, nrow=nr, ncol=nc, byrow=TRUE)
  kmat <- matrix((1:nc)-1, nrow=nr, ncol=nc, byrow=TRUE)
  terms <- coeffsmat * cos(2 * pi * kmat * xmat)
  t(apply(terms, 1, cumsum))
}

pcf_coeffs <- function(ell, max_order = 100) {
  stopifnot(max_order >= 1)
  k <- 0:max_order
  invellsqr <- 1/ell^2
  coeffs0 <- 2 * besselI(invellsqr, k) / exp(invellsqr)
  c(0.5 * coeffs0[1], coeffs0[-1])
}

# write a Haskell list containing the contents of d_mls
write_mls <- function(d_mls) {
  pairs <- vapply(1:nrow(d_mls), FUN.VALUE="", FUN=function(i) {
    sprintf("  (%d, %.15e)", d_mls$order[i], d_mls$min_length_scale[i])
  })
  paste("[\n",  paste(pairs, collapse=",\n"), "\n]", sep="")
}

# This function, together with write_mls, is used to create the variable
# mls_table in Periodic.hs.
# minimum length scale for which the error is <= tol when truncating to a given order
min_length_scale <- function(tol, max_order) {
  ells <- length_scales()
  stopifnot(!is.unsorted(ells)) # values should be increasing
  num_ells <- length(ells)
  maxerr_mat <- max_error(max_order, ells)
  stopifnot(all(dim(maxerr_mat) == c(max_order, num_ells)))
  ok <- (maxerr_mat <= tol)
  stopifnot(any(ok))
  first_true <- function(x){ min(which(x)) }
  kmin <- first_true(ok[ , num_ells])
  kmax <- first_true(ok[ , 1])
  k_index <- kmin : kmax
  ell_index <- apply(ok[kmin : kmax, ], 1, first_true)
  for (i in 1:length(k_index)) {
    stopifnot(ok[k_index[i], ell_index[i]])
    stopifnot(ell_index[i] == 1 || !ok[k_index[i], ell_index[i] - 1])
  }
  data.frame(order = k_index, min_length_scale = ells[ell_index])
}

length_scales <- function(num_steps = 1000) {
  # length scale from 0.04 to 3.00; length scales below about 0.04 lead to floating-point
  # overflow, and length scales greater than 3 lead to near-sinusoidals.
  ellmax <- 3.00
  ellmin <- 0.04
  s <- (ellmax / ellmin) ^ (1/num_steps)
  ellmin * s ^ (0:num_steps)
}

max_error <- function(max_order, ells) {
  num_ells <- length(ells)
  num_points <- 1001
  maxerr_mat <- matrix(NA, nrow = max_order, ncol = num_ells)
  x <- seq(0, 0.5, length.out = num_points)
  for (i in 1:num_ells) {
    ell <- ells[i]
    coeffs <- pcf_coeffs(ell, max_order)
    approxvals <- pcf_approxs(coeffs, x)
    pcf_vec <- periodic_covf(ell)(x)
    stopifnot(pcf_vec[1] == 1)
    xratio <- expansion_ratio(pcf_vec)
    fvals <- matrix(pcf_vec, nr=length(x), nc=length(coeffs))
    max_errs <- xratio * matrixStats::colMaxs(abs(approxvals - fvals))
    stopifnot(length(max_errs) == max_order + 1)
    maxerr_mat[ ,i] <- max_errs[-1]
  }
  stopifnot(all(!is.na(maxerr_mat)))
  stopifnot(has_decreasing_rows_cols(maxerr_mat))
  return(maxerr_mat)
}

# ARG: covs is a vector of autocovariances that run from distance 0 to 0.5.
# RETURN: Amount by which we'll need to scale the autocovariances after subtracting the
#   mean, in order for to have an autocovariance of 1 for distance n=0. 
expansion_ratio <- function(covs) {
  mu <- mean(covs)
  1.0 / (covs[1] - mu)
}

has_decreasing_rows_cols <- function(mat, lobound=3e-15) {
  f <- function(x) {
    stopifnot(all(x >= 0))
    #x[x < lobound] <- 0
    diffs <- x[-1] - x[-length(x)]
    if (any(diffs > lobound)) {
      print(x, digits=16)
      print(which(diffs > lobound))
      print(diffs[diffs > lobound])
      return (FALSE)
    }
    else
      return (TRUE)
  }
  for (i in 1:nrow(mat))
    if (!f(mat[i, ]))
      stop("row ", i, " is not decreasing")
  for (j in 1:ncol(mat))
    if (!f(mat[ , j]))
      stop("col ", j, " is not decreasing")
  all(apply(mat, 1, f)) && all(apply(mat, 2, f))
}

trunc_pcf_coeffs <-
function(coeffs, pcf, period, tol=0.01, min_order=1) {
  stopifnot(min_order + 1 <= length(coeffs)) # + 1 for k=0 term
  if (round(period) == period)
    x <- (0:floor(period/2)) / period
  else
    x <- seq(0, 0.5, length.out = 1001)
  approxvals <- pcf_approxs(coeffs, x)
  fvals <- matrix(pcf(x), nr=length(x), nc=length(coeffs))
  stopifnot(all(fvals[1,] == 1))
  max_errors <- matrixStats::colMaxs(abs(approxvals - fvals))
  stopifnot(any(max_errors <= tol))
  len <- min(pmax(min_order + 1, which(max_errors <= tol)))
  
  return(list(maxerr = max_errors[len],
              coeffs = coeffs[1:len]))
}

reduce_aliased_pcf_coeffs <- function(coeffs, period) {
  stopifnot(round(period) == period)
  stopifnot(period >= 2)
  n <- length(coeffs)
  kmax <- period %/% 2
  if (n <= kmax + 1) # 0, ..., kmax
    return(coeffs)

  # Combine all coefficients whose order mod period are the same
  reps <- ceiling(n / period)
  n1 <- period * reps
  if (n1 > n)
    coeffs <- c(coeffs, rep(0, n1 - n))
  stopifnot(length(coeffs) == n1)
  coeffs <- rowSums(matrix(coeffs, nr=period, nc=reps))
  
  k <- 1
  while (k < period - k) {
    coeffs[1 + k] <- coeffs[1 +  k] + coeffs[1 + period - k]
    k <- k + 1
  }
  return(coeffs[1:(1+kmax)])
}

center_and_norm_pcf_coeffs <- function(coeffs) {
  stopifnot(all(coeffs >= 0))
  coeffs[1] <- 0
  coeffs / sum(coeffs)
}

optimized_pcf_coeffs <-
function(period, ell, tol=1e-2, min_order=1) {
  coeffs <- pcf_coeffs(ell, max(100, min_order))
  if (round(period) == period)
    coeffs <- reduce_aliased_pcf_coeffs(coeffs, period)
  v <- trunc_pcf_coeffs(coeffs, periodic_covf(ell), period,
                        tol, min_order)
  coeffs <- center_and_norm_pcf_coeffs(v$coeffs)
  coeffs[-1]
}

fcoeffs <- function(ell, n) {
  j <- 0:n
  x <- 1/ell^2
  retval <- 2 * besselI(x, j) / exp(x)
  retval[1] <- 0.5 * retval[1]
  stopifnot(all(retval >= 0))
  retval
}

plot_periodic_covf <- function(ell, n, center=FALSE) {
  f <- periodic_covf(ell)
  fc <- fcoeffs(ell, n)
  if (center) {
    fc[1] <- 0
    fc <- fc / sum(fc)
  }
  fapprox <- function(x) {
    retval <- fc[1]
    for (j in 1:n) {
      retval <- retval + fc[j+1] * cos(2 * pi * j * x)
    }
    retval
  }
  xmin <- -1/2; xmax <- 1/2
  xseq <- seq(from=xmin, to=xmax, length.out=1001)
  ylim <- range(c(f(xseq), fapprox(xseq))) * 1.05
  maintitle <- sprintf('length scale %g', ell) 
  plot(f, xmin, xmax, ylim=ylim, n=1000, ylab='correlation', xlab='relative distance',
       main=maintitle)
  par(new=TRUE)
  plot(fapprox, xmin, xmax, col="brown", ylim=ylim, n=1000, ylab='correlation', xlab='relative distance',
       main=maintitle)
  lines(c(xmin, xmax), c(0, 0), lty='dotted')
}

plot_periodic_fct <- function(ell, n) {
  fc <- fcoeffs(ell, n)[-1]
  stopifnot(length(fc) == n)
  fc <- fc / sum(fc)
  rrayleigh <- function(n, sigma) {
    x <- rnorm(n, 0, sigma)
    y <- rnorm(n, 0, sigma)
    sqrt(x^2 + y^2)
  }
  save <- par('mfrow')
  par(mfrow=c(1,3), mar=c(4,4,1,2)+0.1)
  for (dummy in 1:3) {
    a <- rrayleigh(n, sqrt(fc))
    psi <- runif(n, 0, 2 * pi)
    xmin <- -1/2; xmax <- 1/2
    f <- function(x) {
      retval <- 0
      for (k in 1:n)
        retval <- retval + a[k] * cos(2 * pi * k * x + psi[k])
      retval
    }
    plot(f, xmin, xmax, n=1001, xlab="", ylab="")
    lines(c(xmin, xmax), c(0,0), col="gray")
  }
  par(mfrow=save)
}


plot_uncentered_periodic_fct <- function(ell, n) {
  fc <- fcoeffs(ell, n)
  stopifnot(length(fc) == n + 1)
  fc <- fc / sum(fc)
  rrayleigh <- function(n, sigma) {
    x <- rnorm(n, 0, sigma)
    y <- rnorm(n, 0, sigma)
    sqrt(x^2 + y^2)
  }
  save <- par('mfrow')
  par(mfrow=c(1,3), mar=c(4,4,1,2)+0.1)
  for (dummy in 1:3) {
    a <- rrayleigh(n+1, sqrt(fc))
    psi <- runif(n+1, 0, 2 * pi)
    xmin <- -1/2; xmax <- 1/2
    f <- function(x) {
      retval <- 0
      for (k in 0:n)
        retval <- retval + a[k+1] * cos(2 * pi * k * x + psi[k+1])
      retval
    }
    plot(f, xmin, xmax, n=1001, xlab="", ylab="")
    lines(c(xmin, xmax), c(0,0), col="gray")
  }
  par(mfrow=save)
}


