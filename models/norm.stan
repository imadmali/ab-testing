data {
  int<lower=0> N;
  vector[N] y;
  real mu_loc;
  real sigma_loc;
  real<lower=0> mu_scale;
  real<lower=0> sigma_scale;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target+= normal_lpdf(y | mu, sigma);
  target+= normal_lpdf(mu | mu_loc, mu_scale);
  target+= normal_lpdf(sigma | sigma_loc, sigma_scale);
}
generated quantities {
  real y_hat[N];
  for (n in 1:N)
    y_hat[n] = normal_rng(mu, sigma);
}
