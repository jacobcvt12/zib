library(VGAM)
library(R2jags)

# https://faculty.franklin.uga.edu/dhall/sites/faculty.franklin.uga.edu.dhall/files/ZIMixed.pdf
set.seed(1)
#n <- 1000
n <- 299
pts <- rpois(n, 100)

# overall beta parameters
a <- 5
b <- 70

# shared probability of structural zero
phi <- 0.1

theta <- rbeta(n, a, b)
pass <- rbinom(n, pts, theta)
#pass <- rzibinom(n, pts, theta, pstr0=phi)

model <- function() {
    mean.rel <- mean(rel)

    for (i in 1:n) {
        var.binom[i] <- theta[i] * (1 - theta[i]) / pts[i]
        rel[i] <- p.to.p.var / (p.to.p.var + var.binom[i])
    }

    p.to.p.var <- a * b / ((a + b + 1) * (a + b) ^ 2)

    for (i in 1:n) {
        pass[i] ~ dbinom(theta[i], pts[i])
    }

    for (i in 1:n) {
        theta[i] ~ dbeta(a, b)
    }

    # priors
    a ~ dunif(0, 1000)
    b ~ dunif(0, 1000)
}

model.data <- c("pass", "pts", "n")
model.inits <- function() {
    inits <- list()
    inits$a <- runif(1, 0, 1000)
    inits$b <- runif(1, 0, 1000)
    return(inits)
}
model.params <- c("a", "b", "p.to.p.var", "mean.rel")

fit <- jags(model.data, model.inits, model.params, model, n.iter=5000)
fit
