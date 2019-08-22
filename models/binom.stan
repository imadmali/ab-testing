data {
  int<lower=0> N;
  int y[N];
  int trials[N];
  real<lower=0> shape1;
  real<lower=0> shape2;
}
parameters {
  real theta;
}
model {
  target+= binomial_lpmf(y | trials, theta);
  target+= beta_lpdf(theta | shape1, shape2);
}
generated quantities {
  int y_hat[N];
  for (n in 1:N)
    y_hat[n] = binomial_rng(trials[n], theta);
}
