library(rstanarm)
library(bayesplot)
library(dplyr)

# We want to determine if the treatment applied to Group B has a greater effect on y compared to Group A.

group <- c(rep(1,10), rep(2,12))
group <- factor(c(rep("A",10), rep("B",12)))
N <- length(group)
x <- sample(c(0,1), N, replace = TRUE)
effect <- c(0,3)

lp <- effect[group] + 1.5*x
y <- rnorm(N, lp, 1)

experiment <- data.frame(y = y,
                         group = factor(group),
                         x = x)

# modeling group effect
fit0 <- stan_glm(y ~ 0 + group, data = experiment, family = gaussian(link="identity"), chains = 4, iter = 1e3)
# modeling group effect and additional variable
fit1 <- stan_glm(y ~ 0 + group + x, data = experiment, family = gaussian(link="identity"), chains = 4, iter = 1e3)
# modeling hierarchical group effect and additional variable
fit2 <- stan_glmer(y ~ 0 + (1|group) + x, data = experiment, family = gaussian(link="identity"), chains = 4, iter = 1e3)

fit0
fit1
fit2
fixef(fit2)
ranef(fit2)

pp_gA <- posterior_predict(fit1, newdata = data.frame(group = factor("A"), x = x))
pp_gB <- posterior_predict(fit1, newdata = data.frame(group = factor("B"), x = x))
pp_gA_x0 <- posterior_predict(fit1, newdata = data.frame(group = factor("A"), x = 0))
pp_gA_x1 <- posterior_predict(fit1, newdata = data.frame(group = factor("A"), x = 1))
pp_gB_x0 <- posterior_predict(fit1, newdata = data.frame(group = factor("B"), x = 0))
pp_gB_x1 <- posterior_predict(fit1, newdata = data.frame(group = factor("B"), x = 1))

par(mfrow = c(2,2))
hist(pp_gA_x0, breaks = 50, xlim = c(-10,10), xlab = "gAx0", main = "Group A (x=0)")
hist(pp_gA_x1, breaks = 50, xlim = c(-10,10), xlab = "gAx1", main = "Group A (x=1)")
hist(pp_gB_x0, breaks = 50, xlim = c(-10,10), xlab = "gBx0", main = "Group B (x=0)")
hist(pp_gB_x1, breaks = 50, xlim = c(-10,10), xlab = "gBx1", main = "Group B (x=1)")
dev.off()

par(mfrow = c(2,1))
hist(pp_gA, breaks = 50, xlim = c(-10,10), xlab = "gA", main = "Group A")
hist(pp_gB, breaks = 50, xlim = c(-10,10), xlab = "gB", main = "Group B")
dev.off()

experiment %>% group_by(group) %>% summarize(median(y))
experiment %>% group_by(x) %>% summarize(median(y))
experiment %>% group_by(group, x) %>% summarize(median(y))
