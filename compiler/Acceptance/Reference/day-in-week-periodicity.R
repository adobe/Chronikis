createSSMs_day.in.week.periodicity <- function(data_, posterior_draws_, ndraws_) {
  if (missing(ndraws_)) ndraws_ <- .Machine$integer.max
  ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)
  lapply(1:ndraws_, function(i_){createSSMs_day.in.week.periodicity_1(data_, posterior_draws_, i_)})
}

createSSMs_day.in.week.periodicity_1 <- function(data_, posterior_draws_, i_) {
  mu_a_ <- data_$mu_a_
  sigma_a_ <- data_$sigma_a_
  sigma_h_scale_ <- data_$sigma_h_scale_
  sigma_p_scale_ <- data_$sigma_p_scale_
  sigma_q_scale_ <- data_$sigma_q_scale_
  raw_0_ <- posterior_draws_$raw_0[i_]
  raw_1_ <- posterior_draws_$raw_1[i_]
  raw_2_ <- posterior_draws_$raw_2[i_]
  rho_ <- posterior_draws_$rho[i_]
  sigma_q_ <- raw_0_ * sigma_q_scale_
  sigma_h_ <- raw_1_ * sigma_h_scale_
  sigma_p_ <- raw_2_ * sigma_p_scale_
  csigma2s_0_ <- sigma_p_ ^ 2L * c(0.6182923634858543, 0.6182923634858543, 0.27566250834204353, 0.27566250834204353, 0.10604512817210211, 0.10604512817210211)
  rhoSqr_0_ <- rho_ ^ 2L
  Z_0_ <- c(1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0)
  H_0_ <- sigma_h_ ^ 2L
  T_0_ <- dlm::bdiag(as.matrix(1.0), chronikis::bdiag_from_arr3(sqrt(1.0 - rhoSqr_0_) * abind::abind(matrix(c(0.6234898018587336, 0.7818314824680297, (-0.7818314824680297), 0.6234898018587336), nrow = 2L), matrix(c((-0.22252093395631434), 0.9749279121818236, (-0.9749279121818236), (-0.22252093395631434)), nrow = 2L), matrix(c((-0.900968867902419), 0.43388373911755823, (-0.43388373911755823), (-0.900968867902419)), nrow = 2L), along = 0L)))
  Q_0_ <- chronikis::diagv(c(c(sigma_q_ ^ 2L), rhoSqr_0_ * csigma2s_0_))
  a0_0_ <- c(mu_a_, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  P0_0_ <- chronikis::diagv(c(c(sigma_a_ ^ 2L), csigma2s_0_))
  dlm::dlm(FF = matrix(Z_0_, nrow = 1L), V = as.matrix(H_0_), GG = T_0_, W = Q_0_, m0 = a0_0_, C0 = P0_0_)
}
