createSSMs_wn <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_wn_1(data_, posterior_draws_, i_)})
}

createSSMs_wn_1 <- function(data_, posterior_draws_, i_) {
  mu_a_ <- data_$mu_a_
  sigma_a_ <- data_$sigma_a_
  sigma_scale_ <- data_$sigma_scale_
  raw_0_ <- posterior_draws_$raw_0[i_]
  sigma_ <- raw_0_ * sigma_scale_
  Z_0_ <- c(1.0)
  H_0_ <- sigma_ ^ 2L
  T_0_ <- as.matrix(1.0)
  Q_0_ <- as.matrix(0.0)
  a0_0_ <- c(mu_a_)
  P0_0_ <- as.matrix(sigma_a_ ^ 2L)
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
