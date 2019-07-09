functions {
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0> sigma_rw_scale_;
  real<lower = 0.0> sigma_st_scale_;
  real mu_;
  real<lower = 0.0> sigma_;
  real<lower = 0.0> sigma_h_scale_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[2] Z_0_ = to_vector({1.0, 1.0});
  vector[2] a0_0_ = to_vector({0.0, mu_});
}
parameters {
  real<lower = 0.0> raw_0_;
  real<lower = 0.0> raw_1_;
  real<lower = 0.0> raw_2_;
}
transformed parameters {
  real sigma_h_ = raw_0_ * sigma_h_scale_;
  real sigma_rw_ = raw_1_ * sigma_rw_scale_;
  real sigma_st_extra_ = raw_2_ * sigma_st_scale_;
}
model {
  real sigma_st_ = sigma_rw_ + sigma_st_extra_;
  real phi_ = sqrt(1.0 - square(sigma_rw_ / sigma_st_));
  real H_0_ = square(sigma_h_);
  matrix[2, 2] T_0_ = diag_matrix(to_vector({phi_, 1.0}));
  matrix[2, 2] Q_0_ = diag_matrix(to_vector({square(sigma_rw_), 0.0}));
  matrix[2, 2] P0_0_ = diag_matrix(to_vector({square(sigma_st_), square(sigma_)}));
  raw_0_ ~ normal(0.0, 1.0) T[0.0, ];
  raw_1_ ~ normal(0.0, 1.0) T[0.0, ];
  raw_2_ ~ normal(0.0, 1.0) T[0.0, ];
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
