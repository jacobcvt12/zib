library(VGAM)
library(R2jags)

# https://faculty.franklin.uga.edu/dhall/sites/faculty.franklin.uga.edu.dhall/files/ZIMixed.pdf
set.seed(1)
n <- 200
pts <- rpois(n, 100)

# overall beta parameters
a <- 25
b <- 5

# shared probability of structural zero
phi <- 0.1

theta <- rbeta(n, a, b)
#pass <- rzibinom(n, pts, theta, pstr0=phi)
pass <- rbinom(n, pts, theta)

model <- function() {
    for (i in 1:n) {
        pass[i] ~ dbinom(theta[i], pts[i])
        theta[i] ~ dbeta(a, b)
    }

    # priors
    a ~ dunif(0, 1000)
    b ~ dunif(0, 1000)
    #phi[1:2] ~ ddirch(
}

model.data <- c("pass", "pts", "n")
model.inits <- function() {
    inits <- list()
    inits$a <- runif(1, 0, 1000)
    inits$b <- runif(1, 0, 1000)
#    inits$phi <- rbeta(1, 1)
    return(inits)
}
model.params <- c("a", "b")
#model.params <- c("a", "b", "phi")

fit <- jags(model.data, model.inits, model.params, model, n.iter=5000)
fit
