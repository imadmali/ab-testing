data {
  int<lower=0> N;
  int y[N];
  int trials[N];
  real theta_loc;
  real<lower=0> theta_scale;
}
parameters {
  real theta;
}
model {
  target+= binomial_lpmf(y | trials, theta);
  target+= normal_lpdf(theta | theta_loc, theta_scale);
}
generated quantities {
  real y_hat[N];
  for (n in 1:N)
    y_hat[n] = binomial_rng(trials[n], theta);
}
