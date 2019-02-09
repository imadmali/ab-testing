library(rstan)

# Difference in means -----------------------------------------------------

md1 <- list(y = rnorm(1e3, 4, 2),
            mu_loc = 0,
            mu_scale = 3,
            sigma_loc = 0,
            sigma_scale = 3)

md2 <- list(y = rnorm(1e3/2, 2, 4),
            mu_loc = 0,
            mu_scale = 3,
            sigma_loc = 0,
            sigma_scale = 3)

fit1 <- stan("models/diff-norm.stan", data = md1, iter = 2e3, chains = 4)
fit2 <- stan("models/diff-norm.stan", data = md2, iter = 2e3, chains = 4)

# Difference in proportions -----------------------------------------------


