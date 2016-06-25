library(R2jags)
library(shinystan)

set.seed(1)
n <- 100

# latent classes
k <- 3
p <- c(1/4, 1/2, 1/4)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)
alpha <- rep(1, k)

# overall beta parameters
a <- c(1, 50, 99)
b <- c(99, 50, 1)

# generate theta
theta <- rbeta(n, a[z], b[z])

model <- function() {
    for (i in 1:n) {
        theta[i] ~ dbeta(a[z[i]], b[z[i]])
        z[i] ~ dcat(pi[])
    }
    
    # priors
    for (cluster in 1:k) {
        a[cluster] <- ((1 - mu[cluster]) / sigma.2[cluster] - 
                       1 / mu[cluster]) * mu[cluster] ^ 2
        b[cluster] <- a[cluster] * (1 / mu[cluster] - 1)

        # prior on sigma^2|mu and mu
        sigma.2[cluster] ~ dunif(0, mu[cluster] * (1 - mu[cluster]))
        mu[cluster] ~ dbeta(1, 1)
    }

    pi ~ ddirch(alpha[])
}

model.data <- c("theta", "alpha", "k", "n")
model.params <- c("a", "b", "pi", "mu")

fit <- jags(model.data, NULL, model.params, model, n.iter=50000)
fit
