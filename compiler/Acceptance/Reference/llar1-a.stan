functions {
  real polynomial(real x, real[] coeffs) {
    real y = 0;
    real xpow = 1;
    for (i in 1:size(coeffs)) {
      y += xpow * coeffs[i];
      xpow *= x;
    }
    return (y);
  }                            
  
  real exponential_mt_rate(real normalized_mean) {
    // This would be simpler if recursion were allowed...
    real mu;
    real sign;
    real res0;
    if (normalized_mean <= 0)
      reject("exponential_mt_rate: parameter must be positive, but is ",
             normalized_mean, ".");
    if (normalized_mean >= 1)
      reject("exponential_mt_rate: parameter must be less than 1, but is ",
             normalized_mean, ".");
    if (normalized_mean > 0.5) {
      mu = 1.0 - normalized_mean;
      sign = -1.0;
    }
    else {
      mu = normalized_mean;
      sign = 1.0;
    }
    if (mu < 4.54545451755986457121e-02)
      res0 = inv(mu);
    else if (mu < 2.10988429519736647721e-01)
      res0 = polynomial(2.10988429519736647721e-01 - mu,
                        { 9.49448609949615329739e-01,
                          1.04866432774455753396e+00,
                          -6.42959435928104205971e+00,
                          3.79762444624978590113e+00,
                          6.11881450074166792774e+01,
                          -1.48309894287500156906e+02,
                          -2.97228418317228170054e+03,
                          6.97728216979455610272e+04,
                          -4.46472170428893645294e+05,
                          8.96230566675862530246e+05 }) / mu;
    else
      res0 = polynomial(0.5 - mu,
                        { -3.36262872290467608517e-06,
                          1.20012123407418513921e+01,
                          -1.06585476744743931632e-01,
                          3.27768615844976523022e+01,
                          -7.77323727482908424236e+01,
                          9.86330022949583849368e+02,
                          -5.95443311922654356749e+03,
                          2.45388908776985881559e+04,
                          -5.40960590256227224017e+04,
                          5.49423597728985769209e+04 });
    return sign * res0;
  }
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0, upper = 1.0> rho_mean_;
  real mu_;
  real<lower = 0.0> sigma_;
  real<lower = 0.0> sigma_h_scale_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[2] Z_0_ = to_vector({1.0, 1.0});
  vector[2] a0_0_ = to_vector({0.0, mu_});
  matrix[2, 2] P0_0_ = diag_matrix(to_vector({square(sigma_), square(1.0e-2 * sigma_)}));
}
parameters {
  real<lower = 0.0> raw_0_;
  real<lower = 0.0, upper = 1.0> rho_;
}
transformed parameters {
  real sigma_h_ = raw_0_ * sigma_h_scale_;
}
model {
  real phi_ = sqrt(1.0 - square(rho_));
  real H_0_ = square(sigma_h_);
  matrix[2, 2] T_0_ = diag_matrix(to_vector({phi_, 1.0}));
  matrix[2, 2] Q_0_ = diag_matrix(to_vector({square(rho_ * sigma_), 0.0}));
  raw_0_ ~ normal(0.0, 1.0) T[0.0, ];
  rho_ ~ exponential(exponential_mt_rate(rho_mean_)) T[, 1.0];
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
