functions {
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0> sigma_q_scale_;
  real<lower = 0.0> sigma_h_scale_;
  real mu_a_;
  real<lower = 0.0> sigma_a_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[1] Z_0_ = rep_vector(1.0, 1);
  matrix[1, 1] T_0_ = rep_matrix(1.0, 1, 1);
  vector[1] a0_0_ = rep_vector(mu_a_, 1);
  matrix[1, 1] P0_0_ = rep_matrix(square(sigma_a_), 1, 1);
}
parameters {
  real<lower = 0.0> raw_0_;
  real<lower = 0.0> raw_1_;
}
transformed parameters {
  real sigma_q_ = raw_0_ * sigma_q_scale_;
  real sigma_h_ = raw_1_ * sigma_h_scale_;
}
model {
  real H_0_ = square(sigma_h_);
  matrix[1, 1] Q_0_ = rep_matrix(square(sigma_q_), 1, 1);
  raw_0_ ~ cauchy(0.0, 1.0) T[0.0, ];
  raw_1_ ~ normal(0.0, 1.0) T[0.0, ];
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
