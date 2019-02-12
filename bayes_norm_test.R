library(rstan)

# Data --------------------------------------------------------------------

x1 <- c(5.820883, 2.667825, 3.332511, 3.388233, 7.976444,
        5.925112, 6.465919, 7.064625, 3.012066, 2.771472)
x2 <- c(6.329095, 4.575028, 1.346890, 6.440587, 7.479409,
        1.736296, 5.091112, 3.865324, 7.728403, 1.792258)
x3 <- c(-2.541337, -2.778205, -1.363428, -4.821875, -2.526202,
        -3.587017, -1.073344, -2.400323,  3.287509,  3.278655)

md1 <- list(N = length(x1),
            y = x1,
            mu_loc = 0,
            mu_scale = 3,
            sigma_loc = 0,
            sigma_scale = 3)

md2 <- list(N = length(x2),
            y = x2,
            mu_loc = 0,
            mu_scale = 3,
            sigma_loc = 0,
            sigma_scale = 3)

md3 <- list(N = length(x3),
            y = x3,
            mu_loc = 0,
            mu_scale = 3,
            sigma_loc = 0,
            sigma_scale = 3)

# Model -------------------------------------------------------------------

fit1 <- stan("models/norm.stan", data = md1, iter = 2e3, chains = 4)
fit2 <- stan("models/norm.stan", data = md2, iter = 2e3, chains = 4)
fit3 <- stan("models/norm.stan", data = md3, iter = 2e3, chains = 4)

samples1 <- as.matrix(fit1)
samples2 <- as.matrix(fit2)
samples3 <- as.matrix(fit3)

# Post-process ------------------------------------------------------------

# quantile(samples1[,"mu"] - samples2[,"mu"], prob = c(0.05,0.95,0.25,0.75))

pdf("figs/norm1_mu.pdf", width = 8, height = 6)
hist(samples1[,"mu"], breaks = 50,
     main = "mu",
     xlab = "mu")
abline(v = mean(samples1[,"mu"]), col = "red", lwd = 3)
dev.off()

pdf("figs/norm1_sigma.pdf", width = 8, height = 6)
hist(samples1[,"sigma"], breaks = 50,
     main = "sigma",
     xlab = "sigma")
abline(v = mean(samples1[,"sigma"]), col = "red", lwd = 3)
dev.off()

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

pdf("figs/norm1.pdf", width = 8, height = 6)
plot_samples(samples1[,"mu"], breaks = 50,
             main = "Marginal posterior of mu1",
             xlab = "mu1")
dev.off()
pdf("figs/norm2.pdf", width = 8, height = 6)
plot_samples(samples1[,"mu"] - samples2[,"mu"], breaks = 50,
             main = "Marginal posterior of mu1 - mu2",
             xlab = "mu1 - mu2")
dev.off()
pdf("figs/norm3.pdf", width = 8, height = 6)
plot_samples(samples1[,"mu"] - samples3[,"mu"], breaks = 50,
             main = "Marginal posterior of mu1 - mu3",
             xlab = "mu1 - mu3")
dev.off()
