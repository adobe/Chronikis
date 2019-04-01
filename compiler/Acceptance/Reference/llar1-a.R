createSSMs_llar1.a <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_llar1.a_1(data_, posterior_draws_, i_)})
}

createSSMs_llar1.a_1 <- function(data_, posterior_draws_, i_) {
  mu_ <- data_$mu_
  sigma_ <- data_$sigma_
  sigma_h_scale_ <- data_$sigma_h_scale_
  raw_0_ <- posterior_draws_$raw_0[i_]
  rho_ <- posterior_draws_$rho[i_]
  sigma_h_ <- raw_0_ * sigma_h_scale_
  phi_ <- sqrt(1.0 - rho_ ^ 2L)
  Z_0_ <- c(1.0, 1.0)
  H_0_ <- sigma_h_ ^ 2L
  T_0_ <- chronikis::diagv(c(phi_, 1.0))
  Q_0_ <- chronikis::diagv(c((rho_ * sigma_) ^ 2L, 0.0))
  a0_0_ <- c(0.0, mu_)
  P0_0_ <- chronikis::diagv(c(sigma_ ^ 2L, (1.0e-2 * sigma_) ^ 2L))
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
