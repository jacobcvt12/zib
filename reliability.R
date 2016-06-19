library(VGAM)
library(R2jags)

# https://faculty.franklin.uga.edu/dhall/sites/faculty.franklin.uga.edu.dhall/files/ZIMixed.pdf
set.seed(1)
n <- 200
pts <- rpois(n, 100)

# overall beta parameters
a <- 5
b <- 10

# shared probability of structural zero
phi <- 0.1

theta <- rbeta(n, a, b)
pass <- rzibinom(n, pts, theta, pstr0=phi)

model <- function() {
    for (i in 1:n) {
        pass[i] ~ dbinom(zi.theta[i], pts[i])
        zi.theta[i] <- theta[i] * z[i] + 0.00001
        theta[i] ~ dbeta(a, b)
        z[i] ~ dbern(phi.inverse)
    }

    # priors
    a ~ dunif(0, 1000)
    b ~ dunif(0, 1000)
    phi <- 1 - phi.inverse
    phi.inverse ~ dbeta(1, 1)
}

model.data <- c("pass", "pts", "n")
model.params <- c("a", "b", "phi")

fit <- jags(model.data, NULL, model.params, model, n.iter=5000)
fit
