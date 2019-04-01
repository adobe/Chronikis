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
  
  matrix[] scale_matrices(real mult, matrix [] matrices) {
    int N = size(matrices);
    if (N == 0)
      return matrices;
    {
      int nr = rows(matrices[1]);
      int nc = cols(matrices[1]);
      matrix[nr, nc] result[N];
      for (i in 1:N)
        result[i] = mult * matrices[i];
      return result;
    }
  }
  
  int sum_lengths(vector [] va) {
    int n = 0;
    for (i in 1:size(va))
      n += num_elements(va[i]);
    return n;
  }
  
  vector vec_append(vector [] va) {
    int n = size(va);
    if (n == 0) return rep_vector(0.0, 0);
    if (n == 1) return va[1];
    {
      int sz = sum_lengths(va);
      vector[sz] v;
      int bi;
      int ei = 0;
      for (k in 1:n) {
        bi = ei + 1;
        ei += num_elements(va[k]);
        v[bi:ei] = va[k];
      }
      return v;
    }
  }
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0> sigma_q_scale_;
  real<lower = 0.0> sigma_h_scale_;
  real mu_a_;
  real<lower = 0.0> sigma_a_;
  real<lower = 0.0, upper = 1.0> rho_mean_;
  real<lower = 0.0> sigma_p_scale_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[7] Z_0_ = to_vector({1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0});
  vector[7] a0_0_ = to_vector({mu_a_, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0});
}
parameters {
  real<lower = 0.0> raw_0_;
  real<lower = 0.0> raw_1_;
  real<lower = 0.0, upper = 1.0> rho_;
  real<lower = 0.0> raw_2_;
}
transformed parameters {
  real sigma_q_ = raw_0_ * sigma_q_scale_;
  real sigma_h_ = raw_1_ * sigma_h_scale_;
  real sigma_p_ = raw_2_ * sigma_p_scale_;
}
model {
  vector[6] csigma2s_0_ = square(sigma_p_) * to_vector({0.6182923634858543, 0.6182923634858543, 0.27566250834204353, 0.27566250834204353, 0.10604512817210211, 0.10604512817210211});
  real rhoSqr_0_ = square(rho_);
  real H_0_ = square(sigma_h_);
  matrix[1 + 3 * 2, 1 + 3 * 2] T_0_ = block_diag({rep_matrix(1.0, 1, 1), block_diag(scale_matrices(sqrt(1.0 - rhoSqr_0_), {to_matrix({0.6234898018587336, 0.7818314824680297, (-0.7818314824680297), 0.6234898018587336}, 2, 2), to_matrix({(-0.22252093395631434), 0.9749279121818236, (-0.9749279121818236), (-0.22252093395631434)}, 2, 2), to_matrix({(-0.900968867902419), 0.43388373911755823, (-0.43388373911755823), (-0.900968867902419)}, 2, 2)}))});
  matrix[1 + 6, 1 + 6] Q_0_ = diag_matrix(vec_append({rep_vector(square(sigma_q_), 1), rhoSqr_0_ * csigma2s_0_}));
  matrix[1 + 6, 1 + 6] P0_0_ = diag_matrix(vec_append({rep_vector(square(sigma_a_), 1), csigma2s_0_}));
  raw_0_ ~ cauchy(0.0, 1.0) T[0.0, ];
  raw_1_ ~ normal(0.0, 1.0) T[0.0, ];
  rho_ ~ exponential(exponential_mt_rate(rho_mean_)) T[, 1.0];
  raw_2_ ~ normal(0.0, 1.0) T[0.0, ];
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
