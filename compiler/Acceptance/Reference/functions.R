createSSMs_functions <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_functions_1(data_, posterior_draws_, i_)})
}

createSSMs_functions_1 <- function(data_, posterior_draws_, i_) {
  n_ <- data_$n_
  x_ <- data_$x_
  mu_ <- log(x_ ^ n_)
  Z_0_ <- c(1.0)
  H_0_ <- 1.0 ^ 2L
  T_0_ <- as.matrix(1.0)
  Q_0_ <- as.matrix(0.0)
  a0_0_ <- c(mu_)
  P0_0_ <- as.matrix(100.0 ^ 2L)
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
