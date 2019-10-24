library(rstanarm)

# rnorm(12,3,1)  # deleteme
group_a <- c(3.432119, 3.727513, 2.288977, 2.284448, 2.573595, 3.078873,
             3.307105, 2.445558, 2.557591, 2.950913, 3.083117, 2.922103)
# rnorm(10,6,1)  # deleteme
group_b <- c(4.448558, 5.803795, 4.538198, 5.105491, 5.348394,
             6.31029, 7.983746, 6.758476, 5.83918, 6.234961)
# rnorm(10,6,3)  # deleteme
group_c <- c(4.546528, 9.018507, 1.208729, 14.651523, 7.48408,
             7.554656, 4.045493, 10.382762, 10.534418, 7.631014)

fit_group_a <- stan_glm(y ~ 1,
                        data = data.frame(y=group_a),
                        family = gaussian(link="identity"),
                        prior_intercept = normal(0, 1, autoscale = FALSE), seed = 123)
fit_group_b <- stan_glm(y ~ 1,
                        data = data.frame(y=group_b),
                        family = gaussian(link="identity"),
                        prior_intercept = normal(0, 1, autoscale = FALSE), seed = 123)
fit_group_c <- stan_glm(y ~ 1,
                        data = data.frame(y=group_c),
                        family = gaussian(link="identity"),
                        prior_intercept = normal(0, 1, autoscale = FALSE), seed = 123)

pp_a <- posterior_predict(fit_group_a)
pp_b <- posterior_predict(fit_group_b)
pp_c <- posterior_predict(fit_group_c)

hist(pp_a, breaks = 50)
hist(pp_b, breaks = 50)
hist(pp_c, breaks = 50)

#' Quantify Overlapping Distributions
#' @param large Posterior predictive samples that have larger range than \code{small}.
#' @param small Posterior predictive samples that have smaller range than \code{large}.
#' @param p Probability to compute credible interval.
#' @return A proportion between 0 and 1 indicating how much of \code{small} is contained in \code{large} given the credible interval specification.
overlap_prop <- function(large, small, p = 1) {
  p_lwr <- (1-p)/2
  p_upr <- 1 - p_lwr
  large_ci <- quantile(large, probs = c(p_lwr, p_upr))
  left <- min(large_ci)
  right <- max(large_ci)
  indxs <- which(small >= left & small <= right)
  return(length(indxs)/length(small))
}

overlap <- function(a, b, p = 1) {
  length_a <- dist(range(a))
  length_b <- dist(range(b))
  if (length_a >= length_b) {
    out <- overlap_prop(a, b, p)
  }
  else if (length_a < length_b) {
    out <- overlap_prop(b, a, p)
  }
  return(out)
}


overlap(pp_a, pp_b, p = 0.8)
overlap(pp_a, pp_c, p = 0)

ci_p <- rev(seq(0.1,1, by = 0.05))

overlap_ab <- sapply(ci_p, function(s){overlap(pp_a, pp_b, s)})
overlap_ac <- sapply(ci_p, function(s){overlap(pp_a, pp_c, s)})

# Interpretation: The business wants to be 100*p percent sure about the range of the predicted metric (credible interval). Given this interval there is a an overlap proportion that quantifies how different the two groups are.
plot(ci_p, overlap_ab, type = "o",
     xaxt = "n", yaxt = "n",
     main = "Group A vs Group B", xlab = "Credible Interval Probability", ylab = "Overlap Proportion")
axis(1, seq(0.1,1,by=0.1))
axis(2, seq(0,1,by=0.1))
abline(v = 0.5, lty = 2)

plot(ci_p, overlap_ac, type = "o",
     xaxt = "n", yaxt = "n",
     main = "Group A vs Group C", xlab = "Credible Interval Probability", ylab = "Overlap Proportion")
axis(2, seq(0,1,by=0.1))
axis(1, seq(0.1,1,by=0.1))
abline(v = 0.5, lty = 2)
