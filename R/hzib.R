library(VGAM)
library(R2jags)

set.seed(1)
n <- 200
pts <- rpois(n, 50)

# overall beta parameters
a <- 1
b <- 10

# shared probability of structural zero
phi <- 0.1

theta <- rbeta(n, a, b)
pass <- rzibinom(n, pts, theta, pstr0=phi)

model <- function() {
    mean.rel <- mean(rel)
    
    for (i in 1:n) {
        rel[i] <- between.var / (between.var + var.binom[i])
        # https://faculty.franklin.uga.edu/dhall/sites/faculty.franklin.uga.edu.dhall/files/ZIMixed.pdf
        var.binom[i] <- (1-phi) * pts[i] * theta[i] * (1 - theta[i] * (1- phi * pts[i]))
        pass[i] ~ dbinom(zi.theta[i], pts[i])
        zi.theta[i] <- theta[i] * z[i] + 0.00001
        theta[i] ~ dbeta(a, b)
        z[i] ~ dbern(phi.inverse)
    }
    
    between.var <- a * b / ((a + b + 1) * (a + b) ^ 2)

    # priors
    a ~ dunif(0, 1000)
    b ~ dunif(0, 1000)
    phi <- 1 - phi.inverse
    phi.inverse ~ dbeta(1, 1)
}

model.data <- c("pass", "pts", "n")
model.params <- c("a", "b", "phi", "mean.rel")

fit <- jags(model.data, NULL, model.params, model, n.iter=10000)
fit
