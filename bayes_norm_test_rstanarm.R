library(rstanarm)

# Data --------------------------------------------------------------------

x1 <- c(5.820883, 2.667825, 3.332511, 3.388233, 7.976444,
        5.925112, 6.465919, 7.064625, 3.012066, 2.771472)
x2 <- c(6.329095, 4.575028, 1.346890, 6.440587, 7.479409,
        1.736296, 5.091112, 3.865324, 7.728403, 1.792258)
x3 <- c(-2.541337, -2.778205, -1.363428, -4.821875, -2.526202,
        -3.587017, -1.073344, -2.400323,  3.287509,  3.278655)

x1 <- data.frame(x = x1)
x2 <- data.frame(x = x2)
x3 <- data.frame(x = x3)

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

fit1 <- stan_glm(x ~ 1, data = x1, prior_intercept = normal(0,3, autoscale = FALSE), prior_aux = normal(0,3, autoscale = FALSE))
fit2 <- stan_glm(x ~ 1, data = x2, prior_intercept = normal(0,3, autoscale = FALSE), prior_aux = normal(0,3, autoscale = FALSE))
fit3 <- stan_glm(x ~ 1, data = x3, prior_intercept = normal(0,3, autoscale = FALSE), prior_aux = normal(0,3, autoscale = FALSE))

samples1 <- as.matrix(fit1$stanfit)
samples2 <- as.matrix(fit2$stanfit)
samples3 <- as.matrix(fit3$stanfit)

pp1 <- posterior_predict(fit1)

# Post-process ------------------------------------------------------------

pdf("figs/norm1_mu.pdf", width = 8, height = 6)
hist(samples1[,"(Intercept)"], breaks = 50,
     main = expression(paste("Marginal Posterior Distribution of ", hat(mu))),
     xlab = expression(hat(mu)))
abline(v = mean(samples1[,"(Intercept)"]), col = "red", lwd = 3)
dev.off()

pdf("figs/norm1_sigma.pdf", width = 8, height = 6)
hist(samples1[,"sigma"], breaks = 50,
     main = expression(paste("Marginal Posterior Distribution of ", hat(sigma))),
     xlab = expression(paste(hat(sigma))))
abline(v = mean(samples1[,"sigma"]), col = "red", lwd = 3)
dev.off()

pdf("figs/norm1_pp.pdf", width = 8, height = 6)
hist(pp1, breaks = 50,
     main = expression(paste("Posterior Predictive Distribution")),
     xlab = expression(paste(hat(x))))
abline(v = mean(pp1), col = "red", lwd = 3)
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
plot_samples(samples1[,"(Intercept)"], breaks = 50,
             main = expression(paste("Marginal Posterior Distribution of ", hat(mu)[1])),
             xlab = expression(paste(hat(mu)[1])))
dev.off()
pdf("figs/norm2.pdf", width = 8, height = 6)
plot_samples(samples1[,"(Intercept)"] - samples2[,"(Intercept)"], breaks = 50,
             main = expression(paste("Marginal Posterior Distribution of ",hat(mu)[1] , " - ", hat(mu)[2])),
             xlab =  expression(paste(hat(mu)[1] , " - ", hat(mu)[2])))
dev.off()
pdf("figs/norm3.pdf", width = 8, height = 6)
plot_samples(samples1[,"(Intercept)"] - samples3[,"(Intercept)"], breaks = 50,
             main = expression(paste("Marginal Posterior Distribution of ", hat(mu)[1] , " - ", hat(mu)[3])),
             xlab = expression(paste(hat(mu)[1] , " - ", hat(mu)[3])))
dev.off()
