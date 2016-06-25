library(R2jags)
library(shinystan)

set.seed(1)
n <- 100

# overall beta parameters
a <- 5
b <- 70

# generate theta
theta <- rbeta(n, a, b)

# data and parameters for jags
model.data <- c("theta", "n")
model.params <- c("a", "b")

naive.model <- function() {
    for (i in 1:n) {
        theta[i] ~ dbeta(a, b)
    }
    
    # priors
    a ~ dunif(0, 1000)
    b ~ dunif(0, 1000)
}

smarter.model <- function() {
    for (i in 1:n) {
        theta[i] ~ dbeta(a, b)
    }
    
    # now calculate a and b
    a <- ((1 - mu) / sigma.2 - 1 / mu) * mu ^ 2
    b <- a * (1 / mu - 1)

    # prior on variance sigma^2 | mu
    # sigma^2 = ab / ((a+b+1) * (a+b)^2)
    sigma.2 ~ dunif(0, mu * (1 - mu))

    # prior on mean mu
    # mu = a / (a + b)
    mu ~ dbeta(1, 1)
}

set.seed(1)
fit.naive <- jags(model.data, NULL, model.params, 
                  naive.model, n.iter=1000)
set.seed(1)
# 50 iterations!!
fit.smarter <- jags(model.data, NULL, model.params, 
                    smarter.model, n.iter=50)
fit.naive
fit.smarter
