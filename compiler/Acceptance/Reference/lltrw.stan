functions {
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0> sigma_qa_scale_;
  real<lower = 0.0> sigma_qd_scale_;
  real<lower = 0.0> sigma_h_scale_;
  real mu_a_;
  real<lower = 0.0> sigma_a_;
  real mu_d_;
  real<lower = 0.0> sigma_d_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[2] Z_0_ = to_vector({1.0, 0.0});
  matrix[2, 2] T_0_ = to_matrix({1.0, 0.0, 1.0, 1.0}, 2, 2);
  vector[2] a0_0_ = to_vector({mu_a_, mu_d_});
  matrix[2, 2] P0_0_ = diag_matrix(to_vector({square(sigma_a_), square(sigma_d_)}));
}
parameters {
  real<lower = 0.0> raw_0_;
  real<lower = 0.0> raw_1_;
  real<lower = 0.0> raw_2_;
}
transformed parameters {
  real sigma_qa_ = raw_0_ * sigma_qa_scale_;
  real sigma_qd_ = raw_1_ * sigma_qd_scale_;
  real sigma_h_ = raw_2_ * sigma_h_scale_;
}
model {
  real H_0_ = square(sigma_h_);
  matrix[2, 2] Q_0_ = diag_matrix(to_vector({square(sigma_qa_), square(sigma_qd_)}));
  raw_0_ ~ normal(0.0, 1.0) T[0.0, ];
  raw_1_ ~ normal(0.0, 1.0) T[0.0, ];
  raw_2_ ~ normal(0.0, 1.0) T[0.0, ];
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
