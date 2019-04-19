createSSMs_lltar1.a <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_lltar1.a_1(data_, posterior_draws_, i_)})
}

createSSMs_lltar1.a_1 <- function(data_, posterior_draws_, i_) {
  mu_a_ <- data_$mu_a_
  sigma_a_ <- data_$sigma_a_
  sigma_b_scale_ <- data_$sigma_b_scale_
  sigma_d_ <- data_$sigma_d_
  sigma_h_scale_ <- data_$sigma_h_scale_
  raw_0_ <- posterior_draws_$raw_0[i_]
  raw_1_ <- posterior_draws_$raw_1[i_]
  rho_b_ <- posterior_draws_$rho_b[i_]
  rho_d_ <- posterior_draws_$rho_d[i_]
  sigma_h_ <- raw_0_ * sigma_h_scale_
  phi_b_ <- sqrt(1.0 - rho_b_ ^ 2L)
  sigma_b_ <- raw_1_ * sigma_b_scale_
  phi_d_ <- sqrt(1.0 - rho_d_ ^ 2L)
  Z_0_ <- c(1.0, 0.0, 1.0)
  H_0_ <- sigma_h_ ^ 2L
  T_0_ <- dlm::bdiag(matrix(c(1.0, 0.0, 1.0, phi_d_), nrow = 2L), as.matrix(phi_b_))
  Q_0_ <- chronikis::diagv(c(0.0, (rho_d_ * sigma_d_) ^ 2L, (rho_b_ * sigma_b_) ^ 2L))
  a0_0_ <- c(mu_a_, 0.0, 0.0)
  P0_0_ <- chronikis::diagv(c(sigma_a_ ^ 2L, sigma_d_ ^ 2L, sigma_b_ ^ 2L))
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
