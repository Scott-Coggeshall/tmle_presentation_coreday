library(tmle)

library(foreach)
library(doParallel)
library(doRNG)

set.seed(45)

cl <- makeCluster(10)

registerDoParallel(cl)

B <- 10
n_sims <- 1000


output <- foreach(i = 1:B, .combine = "rbind") %dorng% {
library(tmle)
estimates <- data.frame(main_effects_estimate = rep(NA, n_sims/B), 
                          tmle_estimate = rep(NA, n_sims/B), tmle_estimate_initial = rep(NA, n_sims/B), 
                        tmle_estimate_lower_ci = rep(NA, n_sims/B), tmle_estimate_upper_ci = rep(NA, n_sims/B))
sl_libs <- c("SL.glmnet", "SL.ranger", "SL.earth")

for(j in 1:100){
n <- 1000

W1 <- rbinom(n, size=1, prob=0.2) # binary confounder
W2 <- rbinom(n, size=1, prob=0.5) # binary confounder
W3 <- round(runif(n, min=2, max=7)) # continuous confounder
W4 <- round(runif(n, min=0, max=4)) # continuous confounder
A  <- rbinom(n, size=1, prob= plogis(-0.2 + 1.2*W2 + log(10 * W3) + 0.3*W4 + 0.2*W1*W4))

p <- 3

w <- cbind(W2, W3, W4)

#x <- rbinom(n, size = 1, prob = plogis(w[, 1]^2 + sin(w[, 2]) + 5*w[, 3]))

x <- A
y <- rnorm(n, mean = 1 + x + x*w[, 1] + w[, 1] + x*w[, 2] + x*w[,2 ]^2 + x*w[, 3]*w[, 2] + w[, 2], sd = 2)

# p <- 3
# 
# w <- matrix(rnorm(n*p), nrow = n, ncol = p)
# 
# x <- rbinom(n, size = 1, prob = plogis(w[, 1]^2 + sin(w[, 2]) + 5*w[, 3]))

# y <- rnorm(n, mean = 1 + x + x*w[, 1] + w[, 1] + w[, 2] + w[,2 ]^2 + x*w[, 3]*w[, 2] + w[, 2], sd = 2)

tmle_fit <- tmle(Y = y, A = x, W = w, Q.SL.library = sl_libs, g.SL.library = sl_libs)

tmle_estimate_initial <- mean(tmle_fit$Qinit$Q[, 2]) - mean(tmle_fit$Qinit$Q[, 1])

tmle_lower_ci <- tmle_fit$estimates$ATE$CI[1]
tmle_upper_ci <- tmle_fit$estimates$ATE$CI[2]

lm_fit <- lm(y ~ x + w)

estimates$main_effects_estimate[j] <- coef(lm_fit)["x"]
estimates$tmle_estimate[j] <- tmle_fit$estimates$ATE$psi
estimates$tmle_estimate_initial[j] <- tmle_estimate_initial
estimates$tmle_estimate_lower_ci[j] <- tmle_lower_ci
estimates$tmle_estimate_upper_ci[j] <- tmle_upper_ci

}

estimates 
}

output$sim <- 1:nrow(output)

write.csv(output, file = "simulation_output.csv")

output_long <- output %>% pivot_longer(c(main_effects_estimate, tmle_estimate, tmle_estimate_initial))
