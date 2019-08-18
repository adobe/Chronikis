createSSMs_functions <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_functions_1(data_, posterior_draws_, i_)})
}

createSSMs_functions_1 <- function(data_, posterior_draws_, i_) {
  n_ <- data_$n_
  x_ <- data_$x_
  mu_ <- log(x_ ^ n_)
  a_ <- 3.0 ^ (1.0 / 3.0)
  b_ <- exp(a_)
  c_ <- expm1(b_)
  d_ <- log1p(0.1)
  e_ <- 10L %% 3L
  f1_ <- c(1.0, 2.0) + c(3.0, 4.0)
  f2_ <- c(1.0, 2.0) - c(3.0, 4.0)
  f3_ <- c(1.0, 2.0) * c(3.0, 4.0)
  f4_ <- c(1.0, 2.0) / c(3.0, 4.0)
  g1_ <- matrix(c(a_, c_, b_, d_), nrow = 2L) + matrix(c(1.0, 3.0, 2.0, 4.0), nrow = 2L)
  g2_ <- matrix(c(a_, c_, b_, d_), nrow = 2L) - matrix(c(1.0, 3.0, 2.0, 4.0), nrow = 2L)
  g3_ <- matrix(c(a_, c_, b_, d_), nrow = 2L) * matrix(c(1.0, 3.0, 2.0, 4.0), nrow = 2L)
  g4_ <- matrix(c(a_, c_, b_, d_), nrow = 2L) / matrix(c(1.0, 3.0, 2.0, 4.0), nrow = 2L)
  sigma_ <- a_ + b_ + c_ + d_ + as.numeric(e_)
  Z_0_ <- c(c(1.0), f1_, f3_)
  H_0_ <- sigma_ ^ 2L + a_ + b_
  T_0_ <- dlm::bdiag(as.matrix(1.0), g1_, g2_)
  Q_0_ <- dlm::bdiag(as.matrix(0.0), g2_, g3_)
  a0_0_ <- c(c(mu_), f2_, f4_)
  P0_0_ <- dlm::bdiag(as.matrix(100.0 ^ 2L), g3_, g4_)
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
