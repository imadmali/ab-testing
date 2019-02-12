# Computing the one sample t-test and associated p-value

# Data
x <- c(5.820883, 2.667825, 3.332511, 3.388233, 7.976444,
       5.925112, 6.465919, 7.064625, 3.012066, 2.771472)
# x <- rnorm(10, 4.5, 2)
mu_h <- 4.5

#' @param x Vector of normally distributed data.
#' @param mu Hypothesized mean.
#' @return Vector of length one containing a double.
t_test <- function(x, mu) {
  x_bar <- mean(x)
  x_sd <- sd(x)
  N <- length(x)
  statistic <- (x_bar-mu) / (x_sd/sqrt(N))
  return(statistic)
}

# compute the t-statistic given the data
t_stat <- t_test(x, mu_h)
t.test(x, mu = mu_h)

# resample from the assumed distribution of the data under the null hypothesis
sim_stat <- c()
for (i in 1:1e4) {
  # x_sample <- sample(x, 10, replace = TRUE)
  x_sample <- rnorm(10, 4.5, 2)
  sim_stat[i] <- t_test(x_sample, mu_h)
}

# compute the critical value at alpha-level of significance
alpha <- 0.05
crit <- qt(1-alpha, length(x)-1)

# compute the p-value (simlated and actual)
length(c(which(sim_stat >= abs(t_stat)), which(sim_stat <= -abs(t_stat))))/length(sim_stat)
2 * pt(-abs(t_stat), length(x)-1)

# empirical distribution of t-statistic where data is resampled under the null hypothesis
pdf("figs/ttest_dist.pdf", width = 8, height = 6)
hist(sim_stat, breaks = 50, xlab = "t-statistic", main = "Empirical Distribution ot t-statistic")
abline(v = abs(t_stat), col = "red", lwd = 3)
abline(v = -abs(t_stat), col = "red", lwd = 3)
abline(v = abs(crit), lty = 2, lwd = 3)
abline(v = -abs(crit), lty = 2, lwd = 3)
legend("topleft", c("t-stat","crit value"), col = c("red","black"), lty = c(1,2), lwd = c(3,3), bty = "n")
dev.off()
