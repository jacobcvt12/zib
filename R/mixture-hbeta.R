library(R2jags)
library(shinystan)

# read in data
X <- readRDS("data/binomial.RDS")
n <- length(X)

# "infinite" mixture of beta binomial model
model <- function() {
    for (i in 1:n) {
        X[i] ~ dbinom(theta[i], 100)
        theta[i] ~ dbeta(a[z[i]], b[z[i]])
        z[i] ~ dcat(pi[])
    }
    
    # calculate overall mean and variance
    var.overall <- pi %*% (mu^2 + sigma.2) - mu.overall^2
    mu.overall <- mu %*% pi
    
    # priors
    for (j in 1:k) {
        a[j] <- ((1 - mu[j]) / sigma.2[j] - 1 / mu[j]) * mu[j] ^ 2
        b[j] <- a[j] * (1 / mu[j] - 1)

        # sigma^2 | mu
        sigma.2[j] ~ dunif(0, mu[j] * (1 - mu[j]))
    }
    
    # correct label switching issue
    mu <- sort(mu.tmp)

    # prior on mu
    for (j in 1:k) {
        mu.tmp[j] ~ dbeta(mu.a[j], mu.b[j])
        
        # calculate parameters for component mean
        mu.a[j] <- ((1 - mu.mu[j]) / mu.sigma.2[j] - 
                    1 / mu.mu[j]) * mu.mu[j] ^ 2
        mu.b[j] <- mu.a[j] * (1 / mu.mu[j] - 1)
        
        # individual prior on component mean variance
        mu.sigma.2[j] ~ dunif(0, mu.mu[j] * (1 - mu.mu[j]))
        
        # invidivual prior on component mean mean
        mu.mu[j] ~ dbeta(1, 1)
    }
    
    pi ~ ddirch(alpha[])
}

k <- 20 # number of components approx infinity
alpha <- rep(1, k) # prior on pi

model.data <- c("X", "alpha", "k", "n")
model.params <- c("var.overall", "theta")

# fit model
set.seed(42)
fit <- jags(model.data, NULL, model.params, model, n.iter=2000)

# diagnose model
launch_shinystan(as.shinystan(as.mcmc(fit)))

# save results
saveRDS(fit, "output/inf-mix-beta-binomial.RDS")
