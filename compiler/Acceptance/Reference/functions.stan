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
  real<lower = 0.0> x_;
  int n_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  real mu_ = log(x_ ^ n_);
  real a_ = cbrt(3.0);
  real b_ = exp(a_);
  real c_ = expm1(b_);
  real d_ = log1p(0.1);
  int e_ = 10 % 3;
  vector[2] f1_ = to_vector({1.0, 2.0}) + to_vector({3.0, 4.0});
  vector[2] f2_ = to_vector({1.0, 2.0}) - to_vector({3.0, 4.0});
  vector[2] f3_ = to_vector({1.0, 2.0}) .* to_vector({3.0, 4.0});
  vector[2] f4_ = to_vector({1.0, 2.0}) ./ to_vector({3.0, 4.0});
  matrix[2, 2] g1_ = to_matrix({a_, c_, b_, d_}, 2, 2) + to_matrix({1.0, 3.0, 2.0, 4.0}, 2, 2);
  matrix[2, 2] g2_ = to_matrix({a_, c_, b_, d_}, 2, 2) - to_matrix({1.0, 3.0, 2.0, 4.0}, 2, 2);
  matrix[2, 2] g3_ = to_matrix({a_, c_, b_, d_}, 2, 2) .* to_matrix({1.0, 3.0, 2.0, 4.0}, 2, 2);
  matrix[2, 2] g4_ = to_matrix({a_, c_, b_, d_}, 2, 2) ./ to_matrix({1.0, 3.0, 2.0, 4.0}, 2, 2);
  real sigma_ = a_ + b_ + c_ + d_ + (0.0 + e_);
  vector[1 + 2 + 2] Z_0_ = vec_append({rep_vector(1.0, 1), f1_, f3_});
  real H_0_ = square(sigma_) + a_ + b_;
  matrix[1 + 2 + 2, 1 + 2 + 2] T_0_ = block_diag({rep_matrix(1.0, 1, 1), g1_, g2_});
  matrix[1 + 2 + 2, 1 + 2 + 2] Q_0_ = block_diag({rep_matrix(0.0, 1, 1), g2_, g3_});
  vector[1 + 2 + 2] a0_0_ = vec_append({rep_vector(mu_, 1), f2_, f4_});
  matrix[1 + 2 + 2, 1 + 2 + 2] P0_0_ = block_diag({rep_matrix(square(100.0), 1, 1), g3_, g4_});
}
parameters {
}
transformed parameters {
}
model {
  yy ~ gaussian_dlm_obs(to_matrix(Z_0_), T_0_, rep_vector(H_0_, 1), Q_0_, a0_0_, P0_0_);
}
