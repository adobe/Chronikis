functions {
  int sum_nrows(matrix [] M) {
    int n = 0;
    int N = size(M);
    for (i in 1:N)
      n += rows(M[i]);
    return n;
  }
  
  int sum_ncols(matrix [] M) {
    int n = 0;
    int N = size(M);
    for (i in 1:N)
      n += cols(M[i]);
    return n;
  }
  
  matrix block_diag(matrix [] M) {
    int nr = sum_nrows(M);
    int nc = sum_ncols(M);
    matrix[nr, nc] result = rep_matrix(0, nr, nc);
    int N = size(M);
    int i = 0;
    int j = 0;
    for (k in 1:N) {
      int nrk = rows(M[k]);
      int nck = cols(M[k]);
      result[(i+1):(i+nrk), (j+1):(j+nck)] = M[k];
      i += nrk;
      j += nck;
    }
    return result;
  }
  
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
  real<lower = 0.0, upper = 1.0> rho_d_mean_;
  real<lower = 0.0> sigma_d_;
  real mu_a_;
  real<lower = 0.0> sigma_a_;
  real<lower = 0.0, upper = 1.0> rho_b_mean_;
  real<lower = 0.0> sigma_b_scale_;
  real<lower = 0.0> sigma_h_scale_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[3] Z_0_ = to_vector({1.0, 0.0, 1.0});
  vector[3] a0_0_ = to_vector({mu_a_, 0.0, 0.0});
}
parameters {
  real<lower = 0.0> raw_0_;
  real<lower = 0.0, upper = 1.0> rho_b_;
  real<lower = 0.0> raw_1_;
  real<lower = 0.0, upper = 1.0> rho_d_;
}
transformed parameters {
  real sigma_h_ = raw_0_ * sigma_h_scale_;
  real sigma_b_ = raw_1_ * sigma_b_scale_;
}
model {
  real phi_b_ = sqrt(1.0 - square(rho_b_));
  real phi_d_ = sqrt(1.0 - square(rho_d_));
  real H_0_ = square(sigma_h_);
  matrix[2 + 1, 2 + 1] T_0_ = block_diag({to_matrix({1.0, 0.0, 1.0, phi_d_}, 2, 2), rep_matrix(phi_b_, 1, 1)});
  matrix[3, 3] Q_0_ = diag_matrix(to_vector({0.0, square(rho_d_ * sigma_d_), square(rho_b_ * sigma_b_)}));
  matrix[3, 3] P0_0_ = diag_matrix(to_vector({square(sigma_a_), square(sigma_d_), square(sigma_b_)}));
  raw_0_ ~ normal(0.0, 1.0) T[0.0, ];
  rho_b_ ~ exponential(exponential_mt_rate(rho_b_mean_)) T[, 1.0];
  raw_1_ ~ normal(0.0, 1.0) T[0.0, ];
  rho_d_ ~ exponential(exponential_mt_rate(rho_d_mean_)) T[, 1.0];
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
