library(R2jags)
library(shinystan)

X <- readRDS("data/binomial.RDS")
n <- length(X)
N <- 5 * 10 + 2 # choice according to ohlssen et al 2007

# mixture of beta binomial model
model <- function() {
    # random effects distribution mean and variance
    re.var <- pi %*% (theta^2) - re.mean ^ 2
    re.mean <- pi %*% theta
    
    # random effects
    for (i in 1:n) {
        X[i] ~ dbinom(p[i], 100)
        p[i] <- theta[Z[i]]
        Z[i] ~ dcat(pi[])
    }
    
    # constructive DPP
    
    # stick-breaking prior
    pi[1] <- r[1]
    for (j in 2:(N-1)) {
        pi[j] <- r[j] * (1-r[j-1]) * pi[j-1] / r[j-1]
    }
    
    for (j in 1:(N-1)) {
        r[j] ~ dbeta(1, alpha)
    }
    
    # truncation of DP
    pi[N] <- 1 - sum(pi[1:(N-1)])
    
    # baseline distribution
    for (k in 1:N) {
        theta[k] ~ dbeta(a, b)   
    }
    
    # alpha, beta parameters of baseline calculated
    a <- 1
    b <- 1
    
    # DPP parameter prior
    alpha ~ dunif(0.3, 10)
}

model.data <- c("X", "n", "N")
model.params <- c("p", "re.var")

# fit model
set.seed(42)
fit <- jags(model.data, NULL, model.params, model, 
            n.iter=30000)

# diagnose model
launch_shinystan(as.shinystan(as.mcmc(fit)))

# save results
#saveRDS(fit, "output/dp-binomial.RDS")