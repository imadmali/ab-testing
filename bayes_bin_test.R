library(rstan)


# Data --------------------------------------------------------------------
md <- list(N = 1,
           y = as.array(7),
           trials = as.array(10),
           shape1 = 1,
           shape2 = 1)

# Model -------------------------------------------------------------------

fit1 <- stan("models/binom.stan", data = md, iter = 2e3, chains = 4, control = list(adapt_delta = 0.99))

# Post-process ------------------------------------------------------------

samples1 <- as.matrix(fit1)

quantile(samples1[,"theta"], prob = c(0.05,0.95,0.25,0.75))

plot_samples <- function(s, ...) {
  hist(s, ...)
  abline(v = quantile(s, prob = 0.05), lwd = 3)
  abline(v = quantile(s, prob = 0.95), lwd = 3)
  abline(v = quantile(s, prob = 0.25), lwd = 3, lty = 2)
  abline(v = quantile(s, prob = 0.75), lwd = 3, lty = 2)
  legend("topleft",
         c("90% cred int","50% cred int"),
         col = c("black","black"), lty = c(1,2), lwd = c(3,3),
         bty = "n")
}

pdf("figs/bin1.pdf", width = 8, height = 6)
plot_samples(samples1[,"theta"], breaks = 50,
             main = "Posterior of theta",
             xlab = "mu1", xlim = c(0,1))
dev.off()
