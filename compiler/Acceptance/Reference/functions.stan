functions {
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0> x_;
  int n_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  real mu_ = log(x_ ^ n_);
  vector[1] Z_0_ = rep_vector(1.0, 1);
  real H_0_ = square(1.0);
  matrix[1, 1] T_0_ = rep_matrix(1.0, 1, 1);
  matrix[1, 1] Q_0_ = rep_matrix(0.0, 1, 1);
  vector[1] a0_0_ = rep_vector(mu_, 1);
  matrix[1, 1] P0_0_ = rep_matrix(square(100.0), 1, 1);
}
parameters {
}
transformed parameters {
}
model {
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
