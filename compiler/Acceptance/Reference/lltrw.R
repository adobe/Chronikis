createSSMs_lltrw <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_lltrw_1(data_, posterior_draws_, i_)})
}

createSSMs_lltrw_1 <- function(data_, posterior_draws_, i_) {
  mu_a_ <- data_$mu_a_
  mu_d_ <- data_$mu_d_
  sigma_a_ <- data_$sigma_a_
  sigma_d_ <- data_$sigma_d_
  sigma_h_scale_ <- data_$sigma_h_scale_
  sigma_qa_scale_ <- data_$sigma_qa_scale_
  sigma_qd_scale_ <- data_$sigma_qd_scale_
  raw_0_ <- posterior_draws_$raw_0[i_]
  raw_1_ <- posterior_draws_$raw_1[i_]
  raw_2_ <- posterior_draws_$raw_2[i_]
  sigma_qa_ <- raw_0_ * sigma_qa_scale_
  sigma_qd_ <- raw_1_ * sigma_qd_scale_
  sigma_h_ <- raw_2_ * sigma_h_scale_
  Z_0_ <- c(1.0, 0.0)
  H_0_ <- sigma_h_ ^ 2L
  T_0_ <- matrix(c(1.0, 0.0, 1.0, 1.0), nrow = 2L)
  Q_0_ <- chronikis::diagv(c(sigma_qa_ ^ 2L, sigma_qd_ ^ 2L))
  a0_0_ <- c(mu_a_, mu_d_)
  P0_0_ <- chronikis::diagv(c(sigma_a_ ^ 2L, sigma_d_ ^ 2L))
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
