# load libraries
library(R2jags)
library(shinystan)

# read in data
X <- readRDS("data/binomial.RDS")
n <- length(X)

# model for simple beta binomial
model <- function() {
    for (i in 1:n) {
        # likelihood
        X[i] ~ dbinom(theta[i], 100)

        # binomial probabilities
        theta[i] ~ dbeta(a, b)
    }
    
    # calculate alpha, beta from mean and variance
    a <- ((1 - mu) / sigma.2 - 1 / mu) * mu ^ 2
    b <- a * (1 / mu - 1)

    # note that priors are on mu, sigma^2

    # sigma^2 | mu
    sigma.2 ~ dunif(0, mu * (1 - mu))

    # mu
    mu ~ dbeta(1, 1)
}

model.data <- c("X", "n") # data to pass to JAGS
model.params <- c("sigma.2", "theta") # parameters to save

# fit model
set.seed(42)
fit <- jags(model.data, NULL, model.params, model, n.iter=2000)

# diagnose model
launch_shinystan(as.shinystan(as.mcmc(fit)))

# save results
saveRDS(fit, "output/beta-binomial.RDS")
