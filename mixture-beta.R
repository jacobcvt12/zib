library(R2jags)
library(shinystan)

set.seed(1)
n <- 500

# latent classes
k <- 3
p <- rep(1/3, 3)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)
alpha <- rep(1, k)

# overall beta parameters
a <- c(1, 500, 999)
b <- c(999, 500, 1)

# generate theta
theta <- rbeta(n, a[z], b[z])

model <- function() {
    for (i in 1:n) {
        theta[i] ~ dbeta(a[z[i]], b[z[i]])
        z[i] ~ dcat(pi[])
    }
    
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
        mu.tmp[j] ~ dbeta(1, 1)
    }

    pi ~ ddirch(alpha[])
}

model.data <- c("theta", "alpha", "k", "n")
model.params <- c("a", "b", "pi", "mu")

fit <- jags(model.data, NULL, model.params, model, n.iter=50000)
fit
