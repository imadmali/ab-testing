### Computing the Binomial p-value

# Data
x <- rbinom(1, 10, 0.8)
mu_h <- 0.8

#' @param x Number of successes observed.
#' @param n Number of trials.
#' @param p Hypothesized success rate.
#' @return A list containing the densities evaluated for each success \code{x} given \code{n} and \code{p}.
binomial_p_value <- function(x, n, p) {
  d <- dbinom(0:n, 10, p)
  indx <- which(d <= dbinom(x, n, p))
  p_value <- sum(d[indx])
  return(list(d = d, p_value = p_value))
}

bpv <- binomial_p_value(x, 10, mu_h)

# empirical distribution of t-statistic where data is resampled under the null hypothesis
pdf("figs/bintest_dist.pdf", width = 8, height = 6)
cols <- rep("black", 10)
cols[which(bpv$d <= dbinom(x, 10, mu_h))] <- "red"
barplot(bpv$d, space = 5, col = cols, border = FALSE,
        ylim = c(0,0.3),
        main = "Binomial Density Under Null Hypothesis",
        xlab = "Successes (k)",
        ylab = "Density", names.arg = 0:10)
legend("topleft", c("density contributions to p-value","density"), col = c("red","black"), pch = c(15,15), pt.cex = c(2,2), bty = "n")
dev.off()

